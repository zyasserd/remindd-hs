-- | Lib: the scheduler core.  See README.md.
--
-- microhs: give every 'var' definition a type signature.
module Lib
  ( -- * Building a schedule
    Schedule, Config, remind, provided, onFire, handler
    -- * Typed state
  , Var, var, get, set, modify
    -- * Actions
  , Act, io, now, callback, shell, succeeds, readIntCmd
  , snooze, sustained, overdue, done
    -- * Time units
  , second, minute, hour, day, week
    -- * Extension: desktop notifications
  , Button, button, notify
    -- * Running
  , defaultMain
  ) where

import Control.Monad (foldM)
import Control.Exception (finally)
import System.Process (system)
import System.Exit (ExitCode(..), exitSuccess)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, removeFile)
import System.Environment (lookupEnv, getArgs)
import System.IO (openTempFile, hClose, hPutStrLn, stderr)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import Text.Read (readMaybe)


--------------------------------------------------------------------------------
-- 1. BUILDER
--
-- One writer monad builds everything declarative:
--   Schedule = Build Reminder                 (a log of reminders)
--   Config   = Build (Reminder -> Reminder)   (a log of edits to one reminder)
-- Running a block just collects what each statement said; nothing is mutated.
--------------------------------------------------------------------------------

newtype Build e a = Build ([e], a)

collect :: Build e a -> [e]
collect (Build (w, _)) = w

tell :: e -> Build e ()
tell e = Build ([e], ())

instance Functor (Build e) where
  fmap f (Build (w, a)) = Build (w, f a)

instance Applicative (Build e) where
  pure a = Build ([], a)
  Build (w1, f) <*> Build (w2, a) = Build (w1 ++ w2, f a)

instance Monad (Build e) where
  Build (w, a) >>= k = let Build (w', b) = k a in Build (w ++ w', b)


--------------------------------------------------------------------------------
-- 2. TYPED STATE
--
-- A reminder's state is a bag of @key -> text@ fields.  A 'Var' is a typed
-- key with a default; 'get' and 'set' go through 'Show' and 'Read', so state
-- is declared once as a value and then used at its proper type.
--------------------------------------------------------------------------------

type Fields = [(String, String)]

data Var a = Var String a

-- | A named slot of any 'Show'/'Read' type, with its initial value.
var :: String -> a -> Var a
var = Var

peek :: Read a => Var a -> Fields -> a
peek (Var k d) fs = fromMaybe d (lookup k fs >>= readMaybe)

poke :: Show a => Var a -> a -> Fields -> Fields
poke (Var k _) a = upsert k (show a)

upsert :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
upsert k v kvs = (k, v) : filter ((/= k) . fst) kvs

-- Slots the core itself uses.
lastCheck, lastDone, snoozedUntil, streak, lastNotifiedAt :: Var Int
lastCheck      = var "lastCheck" 0       -- when last evaluated
lastDone       = var "lastDone" 0        -- when the user last acknowledged it
snoozedUntil   = var "snoozedUntil" 0    -- skip evaluation until this instant
streak         = var "streak" 0          -- consecutive successes ('sustained')
lastNotifiedAt = var "lastNotifiedAt" 0  -- when a notification was last sent


--------------------------------------------------------------------------------
-- 3. ACTIONS
--
-- Act = Reader Env (State Fields IO), written out by hand.  Its state is the
-- /current reminder's/ fields only, so reminders are isolated by construction.
--------------------------------------------------------------------------------

data Env = Env
  { envNow  :: Int      -- the instant of this run
  , envName :: String   -- the reminder being run
  , envProg :: String   -- how to re-invoke this program
  }

newtype Act a = Act { runAct :: Env -> Fields -> IO (a, Fields) }

instance Functor Act where
  fmap f (Act g) = Act $ \e s -> do
    (a, s') <- g e s
    return (f a, s')

instance Applicative Act where
  pure a = Act $ \_ s -> return (a, s)
  Act f <*> Act g = Act $ \e s -> do
    (h, s') <- f e s
    (a, s'') <- g e s'
    return (h a, s'')

instance Monad Act where
  Act g >>= k = Act $ \e s -> do
    (a, s') <- g e s
    runAct (k a) e s'

io :: IO a -> Act a
io m = Act $ \_ s -> m >>= \a -> return (a, s)

now :: Act Int
now = Act $ \e s -> return (envNow e, s)

get :: Read a => Var a -> Act a
get v = Act $ \_ s -> return (peek v s, s)

set :: Show a => Var a -> a -> Act ()
set v a = Act $ \_ s -> return ((), poke v a s)

modify :: (Read a, Show a) => Var a -> (a -> a) -> Act ()
modify v f = get v >>= set v . f

-- | The shell command that runs this reminder's handler (see 'handler').
callback :: String -> Act String
callback h = Act $ \e s ->
  return (unwords (map shq [envProg e, "run", envName e, h]), s)

-- | Run a shell command, ignoring the result.
shell :: String -> Act ()
shell c = io (system c >> return ())

-- | Run a shell command; true if it exits successfully.
succeeds :: String -> Act Bool
succeeds c = io (fmap (== ExitSuccess) (system c))

-- | Run a shell command and parse its stdout as an integer.
readIntCmd :: String -> Act (Maybe Int)
readIntCmd c = io (readInt c)

-- | Silence this reminder for @d@ seconds: evaluation is skipped entirely
-- until then.
snooze :: Int -> Act ()
snooze d = now >>= \t -> set snoozedUntil (t + d)

-- | Mark this reminder done now: clears its snooze and streak and records the
-- acknowledgement time.  Notifying does /not/ do this (see 'fire'), so a
-- reminder keeps coming back until an action calls 'done'.
done :: Act ()
done = do
  t <- now
  set lastDone t
  set lastCheck t
  set snoozedUntil 0
  set streak 0

-- | A guard that holds while the reminder has not been done for @period@
-- seconds.  A never-done reminder is overdue immediately.  Pair it with a
-- short evaluation period to nag until 'done'.
overdue :: Int -> Act Bool
overdue period = do
  t <- now
  d <- get lastDone
  return (t - d > period)

-- | A guard that holds once @test@ has held @n@ evaluations in a row.
-- (Uses 'streak', so one per reminder; write your own with another 'Var'.)
sustained :: Int -> Act Bool -> Act Bool
sustained n test = do
  ok <- test
  c <- fmap (\k -> if ok then k + 1 else 0) (get streak)
  if c >= n then set streak 0 >> return True
            else set streak c >> return False


--------------------------------------------------------------------------------
-- 4. REMINDERS AND THEIR DSL
--------------------------------------------------------------------------------

data Reminder = Reminder
  { rName     :: String
  , rEvery    :: Int                 -- evaluated at most once per period
  , rGuard    :: Act Bool            -- fires only if this holds
  , rFire     :: Act ()              -- what to do when it fires
  , rHandlers :: [(String, Act ())]  -- named actions for @run <name> <h>@
  }

type Schedule a = Build Reminder a
type Config a = Build (Reminder -> Reminder) a

-- | Declare a reminder, evaluated every @period@ seconds.
remind :: String -> Int -> Config () -> Schedule ()
remind name period cfg = tell (foldl (flip ($)) blank (collect cfg))
  where blank = Reminder name period (return True) (return ()) []

-- | Add a guard.  Guards are tried in order; the first false one stops it.
provided :: Act Bool -> Config ()
provided g = tell (\r -> r { rGuard = rGuard r >>= \ok -> if ok then g else return False })

-- | Add an action to run when the reminder fires.
onFire :: Act () -> Config ()
onFire a = tell (\r -> r { rFire = rFire r >> a })

-- | Register a named action, runnable as @prog run <reminder> <name>@.  A
-- handler is a side action: it does not touch the reminder's schedule.  If
-- a name is registered twice the later one wins.
handler :: String -> Act () -> Config ()
handler n a = tell (\r -> r { rHandlers = upsert n a (rHandlers r) })

second, minute, hour, day, week :: Int
second = 1
minute = 60 * second
hour   = 60 * minute
day    = 24 * hour
week   = 7 * day


--------------------------------------------------------------------------------
-- 5. ENGINE
--
-- One pure function says when a reminder is next due; one 'Act' says what
-- evaluating it means.  The rest is plumbing: load, run each reminder against
-- its own slice of the store, save.
--------------------------------------------------------------------------------

-- | The earliest instant a reminder may be evaluated again: at most once per
-- period, and never while snoozed.
nextEval :: Reminder -> Fields -> Int
nextEval r fs = max (peek snoozedUntil fs) afterPeriod
  where
    checked = peek lastCheck fs
    afterPeriod = if checked == 0 then 0 else checked + rEvery r

-- | Notify now.  Firing does /not/ mark the reminder done -- only 'done' does
-- -- so, evaluated once per period, a reminder keeps nagging until it is
-- acknowledged (or snoozed).
fire :: Reminder -> Act ()
fire r = do
  rFire r
  now >>= set lastNotifiedAt

-- | What one scheduled evaluation means: run the guards, fire if they hold,
-- and remember that we looked, so the next look is a period away.
evaluate :: Reminder -> Act ()
evaluate r = do
  go <- rGuard r
  if go then fire r else return ()
  now >>= set lastCheck

-- | Every reminder's private fields, by name.
type Store = [(String, Fields)]

fieldsOf :: String -> Store -> Fields
fieldsOf n = fromMaybe [] . lookup n

-- | Run an 'Act' against one reminder's slice of the store.
runOn :: Reminder -> Int -> String -> Act a -> Store -> IO (a, Store)
runOn r t prog act st = do
  (a, fs) <- runAct act (Env t (rName r) prog) (fieldsOf (rName r) st)
  return (a, upsert (rName r) fs st)

-- | Run an update on the store under the lock: load, transform, save.
transact :: FilePath -> (Int -> Store -> IO Store) -> IO ()
transact dir f = withLock dir $ do
  st <- readStore (dir ++ "/state")
  t <- nowSeconds
  f t st >>= writeStore (dir ++ "/state")

-- | One timer tick: evaluate every reminder that is due.
check :: FilePath -> String -> [Reminder] -> IO ()
check dir prog rs = transact dir $ \t st0 -> foldM (tick t) st0 rs
  where
    tick t st r
      | nextEval r (fieldsOf (rName r) st) > t = return st
      | otherwise = fmap snd (runOn r t prog (evaluate r) st)

-- | @run <name>@ acknowledges a reminder (runs 'done'); @run <name> <handler>@
-- runs one of its notification buttons, which may itself call 'done' or
-- 'snooze'.
runCmd :: FilePath -> String -> [Reminder] -> String -> Maybe String -> IO ()
runCmd dir prog rs name mh =
  case lookup name [(rName r, r) | r <- rs] of
    Nothing -> die ("unknown reminder: " ++ name)
    Just r -> case mh of
      Nothing -> transact dir $ \t st -> fmap snd (runOn r t prog done st)
      Just h  -> case lookup h (rHandlers r) of
        Nothing  -> die ("unknown handler: " ++ h)
        Just act -> transact dir $ \t st -> fmap snd (runOn r t prog act st)

-- | Forget a reminder (its last run, snooze and streak), so it is due again
-- on the next check.  The reminder must exist; this only clears its state.
resetCmd :: FilePath -> [Reminder] -> String -> IO ()
resetCmd dir rs name =
  case lookup name [(rName r, r) | r <- rs] of
    Nothing -> die ("unknown reminder: " ++ name)
    Just _  -> transact dir $ \_ st -> return (upsert name [] st)

-- | One aligned row per reminder.  Raw epoch seconds say nothing, so every
-- time is a short duration relative to now, and the columns are the things a
-- nagging scheduler actually cares about:
--
-- * @every@       -- how often it is evaluated (its period);
-- * @state@       -- @snoozed <for>@, @due@ (will be looked at on the next
--                    check), or @in <time>@ until it is;
-- * @last done@   -- when the user last acknowledged it ('done');
-- * @last nagged@ -- when it last actually notified (so you can see whether it
--                    is bugging you right now).
--
-- The core cannot know whether the guards will hold, so @state@ is about
-- evaluation, not firing; @last nagged@ is the honest record of firing.
list :: FilePath -> [Reminder] -> IO ()
list dir rs = do
  st <- readStore (dir ++ "/state")
  t <- nowSeconds
  mapM_ putStrLn (table (rows t rs st))

-- | Header plus one row per reminder; empty when there is nothing to show.
-- Pure, so the presentation can be checked without a clock or a store.
rows :: Int -> [Reminder] -> Store -> [[String]]
rows t rs st = case rs of
  [] -> []
  _  -> header : map (\r -> row t r (fieldsOf (rName r) st)) rs
  where header = ["name", "every", "state", "last done", "last nagged"]

row :: Int -> Reminder -> Fields -> [String]
row t r fs =
  [ rName r
  , humanizeExact (rEvery r)
  , status t r fs
  , ago t (peek lastDone fs)
  , ago t (peek lastNotifiedAt fs)
  ]

-- | @snoozed <for>@ / @due@ / @in <time>@ -- about evaluation, not firing.
status :: Int -> Reminder -> Fields -> String
status t r fs
  | su > t    = "snoozed " ++ humanize (su - t)
  | nx <= t   = "due"
  | otherwise = "in " ++ humanize (nx - t)
  where su = peek snoozedUntil fs
        nx = nextEval r fs

-- | A past instant as @<duration> ago@, or @never@ if unset.
ago :: Int -> Int -> String
ago t v
  | v <= 0    = "never"
  | t <= v    = "just now"
  | otherwise = humanize (t - v) ++ " ago"

-- | Left-align columns, one space apart, each wide enough for every row
-- (header included); trailing padding is trimmed.
table :: [[String]] -> [String]
table ws = map (trim . unwords . zipWith pad widths) ws
  where
    widths = foldr (zipWith max) (repeat 0) (map (map length) ws)
    pad w s = s ++ replicate (w - length s) ' '

-- | A duration as @k@ of its largest non-zero units.
humanizeN :: Int -> Int -> String
humanizeN k n
  | n <= 0 = "now"
  | otherwise = case take k (filter ((> 0) . fst) (unitParts n)) of
      [] -> "<1s"
      ps -> unwords [show v ++ u | (v, u) <- ps]

-- | Seconds broken into @(count, unit)@ pairs, largest first.
unitParts :: Int -> [(Int, String)]
unitParts n = [ (n `div` week, "w")
              , (n `div` day `mod` 7, "d")
              , (n `div` hour `mod` 24, "h")
              , (n `div` minute `mod` 60, "m")
              , (n `mod` 60, "s")
              ]

-- | A duration as its single largest unit: @3d@, @4h@, @45m@, @9s@.  One
-- significant unit keeps relative times calm (@in 1d@, not @in 23h 59m@).
humanize :: Int -> String
humanize = humanizeN 1

-- | A period as its two largest units: @1w@, @1d@, @1h 30m@.  Not rounded,
-- because it is a fixed config value rather than a time relative to now.
humanizeExact :: Int -> String
humanizeExact = humanizeN 2

-- | The timer period: the shortest reminder period.  A reminder runs at the
-- first tick at or after it is due, so ticking here never misses one; the
-- only cost is that a period which is not a multiple of it fires slightly
-- late.  There is no sensible timer for an empty schedule, so that is an
-- error (which surfaces as a build failure when Nix derives the period).
-- TODO: might not be the best idea to use minimum, but gcd is not efficient either
interval :: [Reminder] -> IO ()
interval rs = case map rEvery rs of
  [] -> die "the schedule defines no reminders"
  ps -> print (minimum ps)


--------------------------------------------------------------------------------
-- 6. OUTSIDE WORLD: clock, state file, lock, shell quoting
--
-- All the platform-specific code lives here.  microhs has no flock or
-- removeDirectory, so the lock goes through the shell.
--------------------------------------------------------------------------------

-- | Report an error and stop.  There is deliberately no nonzero status: microhs
-- exits 1 with an @uncaught exception: ...@ report for any 'ExitCode' but
-- @ExitSuccess@, and has no @exitImmediately@, so a quiet nonzero exit would
-- need a foreign import.  A clean @exitSuccess@ (which flushes stderr) keeps
-- both compilers quiet; 'interval' callers detect the empty output instead.
die :: String -> IO a
die msg = hPutStrLn stderr ("remindd: " ++ msg) >> exitSuccess

nowSeconds :: IO Int
nowSeconds = readInt "date +%s" >>= maybe (die "cannot read the clock") return

-- | Run a shell command and read its stdout.  Goes through a temp file rather
-- than 'readProcess', because 'readProcess' means different things under
-- microhs and GHC; 'system' always runs @/bin/sh -c@.
readCmd :: String -> IO String
readCmd c = do
  (p, h) <- openTempFile "/tmp" "remindd"
  hClose h
  _ <- system (c ++ " 2>/dev/null > " ++ shq p)
  s <- readFile p
  removeFile p
  return (trim s)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Run a shell command and parse its stdout as an integer.
readInt :: String -> IO (Maybe Int)
readInt c = fmap readMaybe (readCmd c)

-- | One line per field: @reminder key value@.  Names and keys have no spaces;
-- values may (a 'show'n value never contains a raw newline).
readStore :: FilePath -> IO Store
readStore path = do
  ex <- doesFileExist path
  if not ex then return [] else fmap (foldr addLine [] . lines) (readFile path)
  where
    addLine l st = case (word l, word (snd (word l))) of
      ((n, _), (k, v)) | not (null n || null k) ->
        upsert n ((k, v) : fromMaybe [] (lookup n st)) st
      _ -> st
    word s = let (a, b) = break (== ' ') s in (a, drop 1 b)

-- | Write to a temp file, then @mv@ (an atomic rename) over the real one, so
-- a crash never leaves a half-written store.  Safe under the lock.
writeStore :: FilePath -> Store -> IO ()
writeStore path st = do
  writeFile tmp (unlines [n ++ " " ++ k ++ " " ++ v | (n, fs) <- st, (k, v) <- fs])
  _ <- system ("mv " ++ shq tmp ++ " " ++ shq path)
  return ()
  where tmp = path ++ ".tmp"

-- | Single-quote a string for @sh@.
shq :: String -> String
shq s = "'" ++ concatMap esc s ++ "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

-- | Mutual exclusion by an atomically-claimed lock file.  @set -C@ (noclobber)
-- makes the shell create and write the file in one @O_EXCL@-guarded step, so
-- the lock never exists without its timestamp; a holder older than 30s is
-- stale and may be stolen.  The lock is always released, even if @act@ throws,
-- so a crash does not leave the next run waiting for the staleness timeout.
withLock :: FilePath -> IO () -> IO ()
withLock dir act = do
  createDirectoryIfMissing True dir
  ok <- acquire lock
  if not ok then die "another instance holds the lock"
            else act `finally` release
  where
    lock = dir ++ "/.lock"
    release = do
      _ <- system ("rm -rf " ++ shq lock)
      return ()

acquire :: FilePath -> IO Bool
acquire lock = do
  rc <- system ("set -C; date +%s 2>/dev/null > " ++ shq lock)
  case rc of
    ExitSuccess -> return True
    _ -> do
      stale <- isStale lock
      if stale then system ("rm -rf " ++ shq lock) >> acquire lock
               else return False

-- | A lock is stale if it is gone (someone else will re-claim it) or its
-- timestamp is over 30s old.  A present but non-numeric file means its holder
-- is between create and write, so it is /not/ stale -- never steal that.
isStale :: FilePath -> IO Bool
isStale lock = do
  ex <- doesFileExist lock
  if not ex then return True else do
    s <- readFile lock
    t <- nowSeconds
    case readMaybe (trim s) of
      Just ts -> return (t - ts > 30)
      Nothing -> return False


--------------------------------------------------------------------------------
-- 7. EXTENSION: DESKTOP NOTIFICATIONS
--
-- Built only on the core's public hooks: 'onFire' sends the notification,
-- 'handler' registers what each button does, 'callback' yields the shell
-- command that triggers a handler.  The core never mentions any of this.
--------------------------------------------------------------------------------

-- | A notification button: an id (no @=@), a label, and what clicking it does.
data Button = Button
  { btnId    :: String
  , btnLabel :: String
  , btnAct   :: Act ()
  }

button :: String -> String -> Act () -> Button
button = Button

-- | Notify with title, body and buttons whenever the reminder fires.
notify :: String -> String -> [Button] -> Config ()
notify title body bs = do
  mapM_ (\b -> handler (btnId b) (btnAct b)) bs
  onFire $ do
    cbs <- mapM (callback . btnId) bs
    io (system (pipeline title body (zip bs cbs)) >> return ())

-- | One detached shell pipeline:
--
-- > ( k=$(notify-send --app-name=remindd \
-- >       -h string:desktop-entry:remindd --action=id=Label --wait TITLE BODY)
-- >   case "$k" in 'id') <callback>;; esac ) </dev/null >/dev/null 2>&1 &
--
-- @--wait@ blocks until the notification closes and prints the clicked
-- action; backgrounding keeps that from stalling the timer that called us.
-- The fixed @--app-name@ (and @desktop-entry@) make GNOME group every
-- reminder's notifications under a single @remindd@ app instead of the
-- default @notify-send@.
pipeline :: String -> String -> [(Button, String)] -> String
pipeline title body bcs =
  "( k=$(notify-send "
    ++ "--app-name=remindd -h " ++ shq "string:desktop-entry:remindd" ++ " "
    ++ concatMap (action . fst) bcs ++ "--wait -- "
    ++ shq title ++ " " ++ shq body ++ "); "
    ++ "case \"$k\" in " ++ concatMap arm bcs
    ++ "esac ) </dev/null >/dev/null 2>&1 &"
  where
    action b = "--action=" ++ shq (btnId b ++ "=" ++ btnLabel b) ++ " "
    arm (b, cb) = shq (btnId b) ++ ") " ++ cb ++ ";; "


--------------------------------------------------------------------------------
-- 8. MAIN
--------------------------------------------------------------------------------

-- | Reject a schedule that cannot run: a non-positive period (it would make
-- the derived timer 0 and re-evaluate every tick) or two reminders sharing a
-- name (they would silently share one state slot in the store).
validate :: [Reminder] -> IO ()
validate rs = do
  mapM_ periodOk rs
  case duplicates (map rName rs) of
    []  -> return ()
    d:_ -> die ("duplicate reminder name: " ++ d)
  where
    periodOk r
      | rEvery r > 0 = return ()
      | otherwise = die ("reminder " ++ rName r ++ ": period must be positive")

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs) = if x `elem` xs then x : duplicates xs else duplicates xs

-- | Dispatch @check | list | interval | reset <name> | run <name> [<handler>]@.
-- The program path used in callbacks can be overridden with @REMINDD_PROG@;
-- otherwise the executable is assumed to be named @remindd@ (what Nix
-- installs, so it is on @PATH@).
defaultMain :: Schedule a -> IO ()
defaultMain reminders = do
  args <- getArgs
  prog <- fmap (fromMaybe "remindd") (lookupEnv "REMINDD_PROG")
  let rs = collect reminders
  validate rs
  case args of
    ["interval"] -> interval rs
    _ -> do
      dir <- stateDir
      case args of
        ["check"] -> check dir prog rs
        ["list"] -> list dir rs
        ["reset", name] -> resetCmd dir rs name
        ["run", name] -> runCmd dir prog rs name Nothing
        ["run", name, h] -> runCmd dir prog rs name (Just h)
        _ -> usage

-- | The command line help, as an error.
usage :: IO a
usage = die "usage: remindd check | list | interval | reset <name> | run <name> [<handler>]"

-- | The state directory: @$XDG_STATE_HOME/remindd@, or @~/.local/state/remindd@
-- when that is unset.  The store is the file @<dir>/state@.
stateDir :: IO FilePath
stateDir = do
  mx <- lookupEnv "XDG_STATE_HOME"
  case mx of
    Just d | not (null d) -> return (d ++ "/remindd")
    _ -> fmap (++ "/.local/state/remindd") getHomeDirectory
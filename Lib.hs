-- | Lib: the scheduler core.  See README.md.
--
-- microhs: give every 'var' definition a type signature.
module Lib
  ( -- * Building a schedule
    Schedule, Config, remind, provided, onFire, handler, summary
    -- * Typed state
  , Var, var, get, set, modify
  , lastCheck, lastDone, snoozedUntil, streak, lastNotifiedAt
    -- * Display
  , View, get', now', humanize, ago
    -- * Actions
  , Act, io, now, callback, shell, succeeds, readIntCmd
  , lastModified, bundled
  , snooze, sustained, overdue, done
    -- * Logging
  , logLine, logCmd, logPath
    -- * Time units
  , second, minute, hour, day, week
    -- * Extension: desktop notifications
  , Button, button, notify, notifyNow
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
-- One writer monad builds everything: @Schedule = Build Reminder@ (a log of
-- reminders) and @Config = Build (Reminder -> Reminder)@ (edits to one).  A
-- block just collects what its statements said; nothing is mutated.
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
-- 2b. DISPLAY
--
-- A pure read of one reminder's state: @View a@ is @Int -> Fields -> a@ (the
-- instant, then the fields), with no way to run IO or change anything.  It is
-- what 'summary' is built from, so listing can describe a reminder without the
-- power to fire it.
--------------------------------------------------------------------------------

newtype View a = View { runView :: Int -> Fields -> a }

instance Functor View where
  fmap f (View g) = View $ \t fs -> f (g t fs)

instance Applicative View where
  pure a = View $ \_ _ -> a
  View f <*> View g = View $ \t fs -> f t fs (g t fs)

instance Monad View where
  View g >>= k = View $ \t fs -> runView (k (g t fs)) t fs

-- | Read a typed slot for display.
get' :: Read a => Var a -> View a
get' v = View $ \_ fs -> peek v fs

-- | The instant of the evaluation, for display.
now' :: View Int
now' = View $ \t _ -> t


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

-- | Read the environment; read or change the current reminder's state.
-- ('io', 'asks', 'gets' and 'modifies' are the only ways to build an 'Act'.)
asks :: (Env -> a) -> Act a
asks f = Act $ \e s -> return (f e, s)

gets :: (Fields -> a) -> Act a
gets f = Act $ \_ s -> return (f s, s)

modifies :: (Fields -> Fields) -> Act ()
modifies f = Act $ \_ s -> return ((), f s)

now :: Act Int
now = asks envNow

get :: Read a => Var a -> Act a
get v = gets (peek v)

set :: Show a => Var a -> a -> Act ()
set v a = modifies (poke v a)

modify :: (Read a, Show a) => Var a -> (a -> a) -> Act ()
modify v f = get v >>= set v . f

-- | The shell command that runs this reminder's handler (see 'handler').
callback :: String -> Act String
callback h =
  asks (\e -> unwords (map shq [envProg e, "run", envName e, h]))

-- | Absolute path to a helper bundled with the program.  The Nix module
-- installs such helpers in @libexec/remindd@ next to @remindd@ (which lives in
-- @bin@), so we resolve relative to @$REMINDD_PROG@.  This keeps helpers out
-- of @PATH@: they belong to the program, not the environment.
--
-- The program is located with @readlink -f@ first: @command -v@ often yields a
-- symlink (e.g. a profile's @bin/remindd@), and @bin/..@ from the symlink's
-- /directory/ is not the install prefix the helper lives under.
bundled :: FilePath -> Act FilePath
bundled name = do
  p <- asks envProg
  io $ do
    prog <- if '/' `elem` p then return p
                            else readCmd ("command -v " ++ shq p)
    real <- readCmd ("readlink -f " ++ shq prog)
    let bin = if null real then prog else real
    return (dirname (dirname bin) ++ "/libexec/remindd/" ++ name)

-- | Everything up to (not including) the last @'/'@, without a trailing
-- slash; @""@ if there is none.  (@reverse . dropWhile (/= '/') . reverse@
-- alone leaves the separator on the end, and then @dirname@ of an
-- already-slashed path would not shorten it.)
dirname :: FilePath -> FilePath
dirname p = reverse (drop 1 (dropWhile (/= '/') (reverse stripped)))
  where stripped = reverse (dropWhile (== '/') (reverse p))

-- | Run a shell command, ignoring the result.
shell :: String -> Act ()
shell c = io (system c >> return ())

-- | Run a shell command; true if it exits successfully.
success :: String -> IO Bool
success c = fmap (== ExitSuccess) (system c)

-- | 'success' as an 'Act'.
succeeds :: String -> Act Bool
succeeds = io . success

-- | Run a shell command and parse its stdout as an integer.
readIntCmd :: String -> Act (Maybe Int)
readIntCmd = io . readInt

-- | Seconds since the file at @path@ was last modified.  A missing file reads
-- as @0@, so it is never overdue:
--
-- > provided (do age <- lastModified "flake.lock"
-- >              return (age >= 3 * week))
lastModified :: String -> Act Int
lastModified path = do
  t <- now
  m <- readIntCmd ("stat -c %Y " ++ shq path)
  return (maybe 0 (t -) m)

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
-- seconds (i.e. once @now - lastDone >= period@).  A never-done reminder is
-- overdue immediately.  Pair it with a short evaluation period to nag until
-- 'done'.
overdue :: Int -> Act Bool
overdue period = do
  t <- now
  d <- get lastDone
  return (t - d >= period)

-- | A guard that holds once @test@ has held @n@ evaluations in a row.
-- (Uses 'streak', so one per reminder; write your own with another 'Var'.)
sustained :: Int -> Act Bool -> Act Bool
sustained n test = do
  ok <- test
  c <- fmap (\k -> if ok then k + 1 else 0) (get streak)
  if c >= n then set streak 0 >> return True
            else set streak c >> return False


--------------------------------------------------------------------------------
-- 3b. LOGGING
--
-- One shared file for every reminder.  'logLine' writes a message; 'logCmd'
-- runs a command and appends its output.  Both are tagged with the reminder's
-- name, so a single file tells you who did what.
--------------------------------------------------------------------------------

-- | Append a timestamped, name-tagged line to a log file.
logAt :: Env -> FilePath -> String -> IO ()
logAt e p msg = appendLine p ("[" ++ envName e ++ "] " ++ msg)

-- | Append a timestamped line to a log file.  Used by 'logAt' (with a reminder
-- name) and by the lock (which has a state directory but no reminder).
appendLine :: FilePath -> String -> IO ()
appendLine p msg = do
  t <- readCmd "date '+%Y-%m-%d %H:%M:%S'"
  _ <- system ("printf '%s\\n' " ++ shq (t ++ " " ++ msg) ++ " >> " ++ shq p)
  return ()

-- | Run a shell command, appending its output to the shared log; true if it
-- exited successfully.  Every line of the command's output is tagged like a
-- 'logLine' message (with @|@ marking continuation), so even one shared log has
-- no unattributed lines.  The exit status is captured through a temporary file,
-- because the pipeline that does the tagging ends in @awk@, not in the command.
logCmd :: String -> Act Bool
logCmd cmd = Act $ \e s -> do
  p <- logPath
  logAt e p ("run " ++ cmd)
  t <- readCmd "date '+%Y-%m-%d %H:%M:%S'"
  code <- runTagged (t ++ " [" ++ envName e ++ "] | ") p cmd
  let ok = code == Just "0"
  logAt e p ("exit " ++ fromMaybe "?" code)
  return (ok, s)

-- | Run @cmd@, writing every output line to @p@ prefixed with @pre@, and return
-- the command's exit status as text ('Nothing' if it could not be read).  The
-- side channel is a @\/tmp@ file (as in 'readCmd'), removed even on failure.
runTagged :: String -> FilePath -> String -> IO (Maybe String)
runTagged pre p cmd = do
  (st, h) <- openTempFile "/tmp" "remindd.exit"
  hClose h
  _ <- system ( "{ " ++ cmd ++ "; printf '%s' \"$?\" > " ++ shq st ++ "; } 2>&1"
                ++ " | INFO=" ++ shq pre
                ++ " awk '{ print ENVIRON[\"INFO\"] $0 }' >> " ++ shq p )
  code <- readCmd ("cat " ++ shq st) `finally` removeFile st
  return (if null code then Nothing else Just code)

-- | Append one line to the shared log, tagged with this reminder's name.
logLine :: String -> Act ()
logLine msg = Act $ \e s -> do
  p <- logPath
  logAt e p msg
  return ((), s)


--------------------------------------------------------------------------------
-- 4. REMINDERS AND THEIR DSL
--------------------------------------------------------------------------------

data Reminder = Reminder
  { rName     :: String
  , rEvery    :: Int                 -- evaluated at most once per period
  , rGuard    :: Act Bool            -- fires only if this holds
  , rFire     :: Act ()              -- what to do when it fires
  , rHandlers :: [(String, Act ())]  -- named actions for @run <name> <h>@
  , rSummary  :: Int -> Fields -> String  -- one-line description for @list@
  }

type Schedule a = Build Reminder a
type Config a = Build (Reminder -> Reminder) a

-- | Declare a reminder, evaluated every @period@ seconds.
remind :: String -> Int -> Config () -> Schedule ()
remind name period cfg = tell (foldl (flip ($)) blank (collect cfg))
  where blank = Reminder name period (return True) (return ()) [] (\_ _ -> "")

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

-- | Describe this reminder's state in one line, for the @summary@ column of
-- @remindd list@.  Built from 'get'', 'now'' and the remaining core 'Var's, so
-- it is pure: displaying a reminder can never notify or change its state.
--
-- > remind "powerSaver" (10 * minute) $ do
-- >   provided (sustained 3 powerSaverOff)
-- >   summary (do s <- get' streak; return ("streak " ++ show s ++ "/3"))
summary :: View String -> Config ()
summary v = tell (\r -> r { rSummary = runView v })


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
  f t st >>= writeStore dir

-- | One timer tick: evaluate every reminder that is due.
check :: FilePath -> String -> [Reminder] -> IO ()
check dir prog rs = transact dir $ \t st0 -> foldM (tick t) st0 rs
  where
    tick t st r
      | nextEval r (fieldsOf (rName r) st) > t = return st
      | otherwise = fmap snd (runOn r t prog (evaluate r) st)

-- | Find a reminder by name, or stop.
findReminder :: [Reminder] -> String -> IO Reminder
findReminder rs name =
  maybe (die ("unknown reminder: " ++ name)) return (lookup name [(rName r, r) | r <- rs])

-- | @run <name>@ acknowledges a reminder (runs 'done'); @run <name> <handler>@
-- runs one of its notification buttons, which may itself call 'done' or
-- 'snooze'.
runCmd :: FilePath -> String -> [Reminder] -> String -> Maybe String -> IO ()
runCmd dir prog rs name mh = do
  r <- findReminder rs name
  act <- case mh of
    Nothing -> return done
    Just h  -> maybe (die ("unknown handler: " ++ h)) return (lookup h (rHandlers r))
  transact dir $ \t st -> fmap snd (runOn r t prog act st)

-- | Forget a reminder's state (its last run, snooze and streak), so it is due
-- again on the next check.
resetCmd :: FilePath -> [Reminder] -> String -> IO ()
resetCmd dir rs name = do
  _ <- findReminder rs name
  transact dir $ \_ st -> return (upsert name [] st)

-- | One aligned row per reminder.  The columns come in two groups, because a
-- reminder has two clocks.  The /check/ group is the scheduler's side: @every@
-- (its period), @checked@ (when its guards last ran, 'lastCheck') and @next@
-- (@snoozed@/@due@/@in@ -- when it will be looked at again, from 'nextEval').
-- The /act/ group is what has actually happened: @fired@ (when its effects last
-- ran, 'lastNotifiedAt') and @done@ (when you last acknowledged it).  A guard's
-- outcome is unknowable from here, so it is not shown; only a @summary@ can
-- reveal it.  @fired@ and @done@ move independently -- firing does not
-- acknowledge -- which is exactly what a nagging scheduler is about.
list :: FilePath -> [Reminder] -> IO ()
list dir rs = withLockIfFree dir $ do
  st <- readStore (dir ++ "/state")
  t <- nowSeconds
  let ws = rows t rs st
  if null ws then return ()
    else mapM_ putStrLn (table (columnGroups (length (head ws))) ws)

-- | The group label over each column, left to right; @""@ is ungrouped.  Sized
-- to the columns 'rows' emits, with or without the @summary@ column.
columnGroups :: Int -> [String]
columnGroups n = take n ["", "check", "check", "check", "act", "act", "note"]

-- | Header plus one row per reminder; empty when there is nothing to show.
-- Pure, so the presentation can be checked without a clock or a store.  The
-- @summary@ column is dropped entirely when no reminder defines one, so the
-- output stays as it was until a 'summary' is added.
rows :: Int -> [Reminder] -> Store -> [[String]]
rows t rs st = case rs of
  [] -> []
  _  -> if any hasSummary rs then header : body else map init (header : body)
  where
    header = ["name", "every", "checked", "next", "fired", "done", "summary"]
    body = map (\r -> row t r (fieldsOf (rName r) st)) rs
    hasSummary r = not (null (rSummary r t (fieldsOf (rName r) st)))

row :: Int -> Reminder -> Fields -> [String]
row t r fs =
  [ rName r
  , humanizeExact (rEvery r)
  , ago t (peek lastCheck fs)
  , status t r fs
  , ago t (peek lastNotifiedAt fs)
  , ago t (peek lastDone fs)
  , rSummary r t fs
  ]

-- | @snoozed <for>@ / @due@ / @in <time>@ -- the @next@ column: when the
-- scheduler will next evaluate the reminder, not whether it will fire.
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

-- | One row of cells, two spaces apart, each padded to its column width, with
-- trailing padding trimmed.
rowLine :: [Int] -> [String] -> String
rowLine ws = trim . join . zipWith pad ws
  where
    pad w s = s ++ replicate (w - length s) ' '
    join = foldr1 (\a b -> a ++ "  " ++ b)

-- | The group header: each label centred over the columns it spans, a run of
-- the same label merged, and ungrouped columns left blank.
groupLine :: [Int] -> [String] -> String
groupLine ws gs = trim (go 0 gs)
  where
    go _ [] = ""
    go i (g:rest)
      | null g = replicate (ws !! i) ' ' ++ "  " ++ go (i + 1) rest
      | otherwise =
          let same = takeWhile (== g) (g : rest)
              j    = i + length same - 1
              w    = sum (take (length same) (drop i ws)) + 2 * (length same - 1)
          in centered g w ++ "  " ++ go (j + 1) (drop (length same - 1) rest)

-- | Pad @s@ to width @w@, split as evenly as possible on both sides.
centered :: String -> Int -> String
centered s w = let p = max 0 (w - length s)
               in replicate (p `div` 2) ' ' ++ s ++ replicate (p - p `div` 2) ' '

-- | A table: a centred group row, the header, a dashed rule, then the body.
-- Columns are two spaces apart, each wide enough for the header and every row;
-- trailing padding is trimmed.
table :: [String] -> [[String]] -> [String]
table _ [] = []
table gs (header:body) = groupLine ws gs : rowLine ws header : rule : map (rowLine ws) body
  where
    ws = foldr (zipWith max) (repeat 0) (map (map length) (header : body))
    rule = trim (join (map (`replicate` '-') ws))
    join = foldr1 (\a b -> a ++ "  " ++ b)

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
-- first tick at or after it is due, so ticking here never misses one (at worst
-- it fires slightly late).  An empty schedule is an error, which surfaces as a
-- build failure when Nix derives the period.
interval :: [Reminder] -> IO ()
interval rs = case map rEvery rs of
  [] -> die "the schedule defines no reminders"
  ps -> print (minimum ps)


--------------------------------------------------------------------------------
-- 6. OUTSIDE WORLD: clock, state file, shell quoting
--
-- The platform-specific basics everything else is built on: running commands
-- and reading the clock, the state file, and quoting for the shell.
--------------------------------------------------------------------------------

-- | Report an error and stop.  There is deliberately no nonzero status: microhs
-- cannot exit nonzero quietly, so this stays @exitSuccess@ (flushing stderr);
-- 'interval' callers detect the empty output instead.
die :: String -> IO a
die msg = hPutStrLn stderr ("remindd: " ++ msg) >> exitSuccess

nowSeconds :: IO Int
nowSeconds = readInt "date +%s" >>= maybe (die "cannot read the clock") return

-- | Run a shell command and read its stdout.  A temp file, not 'readProcess'
-- (which differs between microhs and GHC); 'system' always runs @/bin/sh -c@.
-- The temp file is removed even if the command or the read throws.
readCmd :: String -> IO String
readCmd c = do
  (p, h) <- openTempFile "/tmp" "remindd"
  hClose h
  s <- (do _ <- system (c ++ " 2>/dev/null > " ++ shq p)
           readFile p) `finally` removeFile p
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

-- | Write the store to a temp file, fsync it, @mv@ (an atomic rename) it over
-- the real one, and fsync the directory, so a crash leaves either the old store
-- or the complete new one -- never a half-written one.  Safe under the lock.
writeStore :: FilePath -> Store -> IO ()
writeStore dir st = do
  writeFile tmp (unlines [n ++ " " ++ k ++ " " ++ v | (n, fs) <- st, (k, v) <- fs])
  _ <- system ("sync " ++ shq tmp
               ++ " && mv " ++ shq tmp ++ " " ++ shq path
               ++ " && sync " ++ shq dir)
  return ()
  where tmp = path ++ ".tmp"
        path = dir ++ "/state"

-- | Single-quote a string for @sh@.
shq :: String -> String
shq s = "'" ++ concatMap esc s ++ "'"
  where
    esc '\'' = "'\\''"
    esc c = [c]

--------------------------------------------------------------------------------
-- 7. LOCK
--
-- Mutual exclusion by an atomically-claimed lock file holding its holder's
-- identity.  Pid alone is not an identity -- pids are reused -- but pid with
-- the process's start time is, so a lock is stale iff its holder is gone: a
-- backup killed mid-run is reclaimed at once, a live holder (however long) is
-- never stolen, and no age limit is needed.
--
-- Claiming is @set -C@ (one atomic create-and-write); stealing a stale lock is
-- an @mv@ (an atomic rename), so exactly one racer wins and no process removes
-- a lock another just created.
--------------------------------------------------------------------------------

-- | A lock's holder: a pid and the process's start time (field 22 of
-- @/proc/<pid>/stat@).  'show' is exactly the lock file's contents and
-- 'holderOf' its inverse, so the file format lives in one place.
data Holder = Holder
  { holderPid   :: Int
  , holderStart :: Int
  }

instance Show Holder where
  show h = show (holderPid h) ++ " " ++ show (holderStart h)

-- | Read a holder back from a lock file's (trimmed) contents.
holderOf :: String -> Maybe Holder
holderOf s = case words s of
  [p, t] -> case (readMaybe p :: Maybe Int, readMaybe t :: Maybe Int) of
    (Just pid, Just start) -> Just (Holder pid start)
    _ -> Nothing
  _ -> Nothing

-- | This process's own id.  Read in-process: through the shell, @/proc/self@
-- would be the id of the shell or @cat@, not of this program.
getPid :: IO Int
getPid = do
  s <- readFile "/proc/self/stat"
  return (fromMaybe 0 (readMaybe (takeWhile (/= ' ') s)))

-- | A process's start time (field 22 of @/proc/<pid>/stat@): take the text
-- after the command field's @)@, then its 20th field.  'Nothing' if it is gone.
procStart :: Int -> IO (Maybe Int)
procStart pid = readInt $
  "awk -F')' '{print $NF}' /proc/" ++ show pid
    ++ "/stat 2>/dev/null | awk '{print $20}'"

-- | This process's own identity.
self :: IO Holder
self = do
  pid <- getPid
  start <- fmap (fromMaybe 0) (procStart pid)
  return (Holder pid start)

-- | Is a recorded holder still running?  Since pids are reused, only a
-- matching start time counts as alive; if it is unknown, fall back to
-- @kill -0@.
holderAlive :: Holder -> IO Bool
holderAlive h
  | holderStart h > 0 = do
      cur <- procStart (holderPid h)
      maybe fallback (\s -> return (s == holderStart h)) cur
  | otherwise = fallback
  where
    fallback =
      success ("kill -0 " ++ show (holderPid h) ++ " 2>/dev/null")

-- | The raw contents of a lock file, empty when it is absent.
readLock :: FilePath -> IO String
readLock lock = fmap trim (readCmd ("cat " ++ shq lock))

-- | Is the lock held by a live process?  Absent or dead-holder means no; a
-- present but unparsable file is treated as held, conservatively.
isHeld :: FilePath -> IO Bool
isHeld lock = do
  s <- readLock lock
  case holderOf s of
    Just h  -> holderAlive h
    Nothing -> return (not (null s))

-- | The @O_EXCL@ create-and-write of our identity.
claim :: FilePath -> Holder -> String
claim lock me =
  "set -C; { printf '%s\\n' " ++ shq (show me) ++ " > " ++ shq lock ++ "; } 2>/dev/null"

-- | Atomically create the lock, stealing it (by atomic rename) only if its
-- holder is gone, so exactly one process takes an abandoned lock.
acquire :: FilePath -> IO Bool
acquire lock = do
  me <- self
  created <- success (claim lock me)
  if created then return True else do
    held <- isHeld lock
    if held then return False else do
      moved <- success ("mv " ++ shq lock ++ " " ++ shq side)
      _ <- system ("rm -rf " ++ shq side)
      if not moved then return False else success (claim lock me)
  where side = lock ++ ".old"

-- | @withLock dir act@ runs @act@ with exclusive access.  A held lock is
-- waited for briefly; the lock is released only while still ours, and always
-- (even if @act@ throws).  If the lock never frees, the skip is logged before
-- 'die' reports it on stderr -- 'die' exits 0, so a skipped run would otherwise
-- be indistinguishable from a completed one.
withLock :: FilePath -> IO () -> IO ()
withLock dir act = do
  createDirectoryIfMissing True dir
  ok <- acquireWithin 5
  if not ok then do
      appendLine (dir ++ "/log") "[remindd] skipped: another instance holds the lock"
      die "another instance holds the lock"
    else act `finally` releaseLock (lockFile dir)
  where
    acquireWithin 0 = return False
    acquireWithin n = acquire (lockFile dir) >>= \ok ->
      if ok then return True
            else system "sleep 1" >> acquireWithin (n - 1)

-- | Run a reader under the lock when it is free; if a writer holds it, run
-- anyway.  Either way the read is safe: the store is replaced by an atomic
-- rename, so an unlocked reader still sees a complete snapshot, and a manual
-- @list@ should not be blocked by a long-running job.
withLockIfFree :: FilePath -> IO a -> IO a
withLockIfFree dir act = do
  createDirectoryIfMissing True dir
  ok <- acquire (lockFile dir)
  if ok then act `finally` releaseLock (lockFile dir) else act

-- | The lock file for a state directory.
lockFile :: FilePath -> FilePath
lockFile dir = dir ++ "/.lock"

-- | Remove the lock, but only while it is still ours.
releaseLock :: FilePath -> IO ()
releaseLock lock = do
  me <- self
  held <- readLock lock
  if held == show me
    then do _ <- system ("rm -f " ++ shq lock); return ()
    else return ()


--------------------------------------------------------------------------------
-- 8. EXTENSION: DESKTOP NOTIFICATIONS
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
  onFire (notifyNow title body bs)

-- | Send a notification right now -- the imperative counterpart of 'notify',
-- for when whether to notify is known only at run time.  It registers nothing,
-- so every button's id must already be registered with 'handler'.
notifyNow :: String -> String -> [Button] -> Act ()
notifyNow title body bs = do
  cbs <- mapM (callback . btnId) bs
  io (system (pipeline title body (zip bs cbs)) >> return ())

-- | One detached shell pipeline: @notify-send --wait@ (which prints the
-- clicked action) run in the background so it does not stall the caller, then
-- the matching callback.  The fixed @--app-name@/@desktop-entry@ make GNOME
-- group every notification under one @remindd@ app.
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
-- 9. MAIN
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
-- Callbacks use @$REMINDD_PROG@, else the executable @remindd@ (what Nix
-- installs).
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

-- | The shared log, @<state dir>/log@: every reminder appends here through
-- 'logLine'/'logCmd', so there is one file to read.
logPath :: IO FilePath
logPath = fmap (++ "/log") stateDir

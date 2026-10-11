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
  , snooze, sustained
    -- * Time units
  , seconds, minutes, hours, days, weeks
    -- * Extension: desktop notifications
  , Button, button, notify
    -- * Running
  , defaultMain
  ) where

import Control.Monad (foldM)
import System.Process (system)
import System.Exit (ExitCode(..), die)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, removeFile)
import System.Environment (lookupEnv, getArgs)
import System.IO (openTempFile, hClose)
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
lastRun, lastDone, snoozedUntil, streak :: Var Int
lastRun      = var "lastRun" 0       -- when last evaluated
lastDone     = var "lastDone" 0      -- when last fired
snoozedUntil = var "snoozedUntil" 0  -- silent until this instant
streak       = var "streak" 0        -- consecutive successes ('sustained')


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

-- | Silence this reminder for @d@ seconds.
snooze :: Int -> Act ()
snooze d = now >>= \t -> set snoozedUntil (t + d)

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

seconds, minutes, hours, days, weeks :: Int
seconds = 1
minutes = 60 * seconds
hours   = 60 * minutes
days    = 24 * hours
weeks   = 7 * days


--------------------------------------------------------------------------------
-- 5. ENGINE
--
-- One pure function says when a reminder is next due; one 'Act' says what
-- evaluating it means.  The rest is plumbing: load, run each reminder against
-- its own slice of the store, save.
--------------------------------------------------------------------------------

-- | The earliest instant a reminder may be evaluated again.
nextEval :: Reminder -> Fields -> Int
nextEval r fs = max (peek snoozedUntil fs) afterPeriod
  where
    ran = peek lastRun fs
    afterPeriod = if ran == 0 then 0 else ran + rEvery r

-- | Fire now: run the effect and restart the period.
fire :: Reminder -> Act ()
fire r = do
  t <- now
  rFire r
  set lastDone t
  set lastRun t

-- | What one scheduled evaluation means.
evaluate :: Reminder -> Act ()
evaluate r = do
  go <- rGuard r
  if go then fire r else now >>= set lastRun

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

-- | @run <name>@ fires the reminder now (skipping its guards);
-- @run <name> <handler>@ runs one of its handlers.
runCmd :: FilePath -> String -> [Reminder] -> String -> Maybe String -> IO ()
runCmd dir prog rs name mh =
  case lookup name [(rName r, r) | r <- rs] of
    Nothing -> die ("unknown reminder: " ++ name)
    Just r -> case maybe (Just (fire r)) (\h -> lookup h (rHandlers r)) mh of
      Nothing  -> die ("unknown handler: " ++ fromMaybe "" mh)
      Just act -> transact dir $ \t st -> fmap snd (runOn r t prog act st)

-- | Per reminder: next evaluation, last fire, snooze.
list :: FilePath -> [Reminder] -> IO ()
list dir rs = do
  st <- readStore (dir ++ "/state")
  t <- nowSeconds
  mapM_ (row st t) rs
  where
    row st t r =
      let fs = fieldsOf (rName r) st
      in putStrLn (rName r
           ++ "\tnext " ++ show (max t (nextEval r fs))
           ++ "\tlastDone " ++ show (peek lastDone fs)
           ++ "\tsnoozedUntil " ++ show (peek snoozedUntil fs))

-- | The timer period: the shortest reminder period.  A reminder runs at the
-- first tick at or after it is due, so ticking here never misses one; the
-- only cost is that a period which is not a multiple of it fires slightly
-- late.  (Prints @none@ if there are no reminders.)
-- TODO: might not be the best idea to use minimum, but gcd is not efficient either
interval :: [Reminder] -> IO ()
interval rs = putStrLn (case map rEvery rs of [] -> "none"; ps -> show (minimum ps))


--------------------------------------------------------------------------------
-- 6. OUTSIDE WORLD: clock, state file, lock, shell quoting
--
-- All the platform-specific code lives here.  microhs has no flock or
-- removeDirectory, so the lock goes through the shell.
--------------------------------------------------------------------------------

nowSeconds :: IO Int
nowSeconds = fmap (fromMaybe 0) (readInt "date +%s")

-- | Run a shell command and read its stdout.  Goes through a temp file rather
-- than 'readProcess', because 'readProcess' means different things under
-- microhs and GHC; 'system' always runs @/bin/sh -c@.
readCmd :: String -> IO String
readCmd c = do
  (p, h) <- openTempFile "/tmp" "sched"
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

-- | Mutual exclusion via @mkdir@ (atomic); a lock older than 30s is stale.
withLock :: FilePath -> IO () -> IO ()
withLock dir act = do
  createDirectoryIfMissing True dir
  ok <- acquire lock
  if not ok then die "remindd: another instance holds the lock" else do
    act
    _ <- system ("rm -rf " ++ shq lock)
    return ()
  where lock = dir ++ "/.lock"

acquire :: FilePath -> IO Bool
acquire lock = do
  rc <- system ("mkdir " ++ shq lock ++ " 2>/dev/null")
  case rc of
    ExitSuccess -> do
      t <- nowSeconds
      writeFile (lock ++ "/created") (show t)
      return True
    _ -> do
      stale <- isStale lock
      if stale then system ("rm -rf " ++ shq lock) >> acquire lock
               else return False

isStale :: FilePath -> IO Bool
isStale lock = do
  ex <- doesFileExist (lock ++ "/created")
  if not ex then return True else do
    s <- readFile (lock ++ "/created")
    t <- nowSeconds
    return (maybe True (\ts -> t - ts > 30) (readMaybe s))


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
-- > ( k=$(notify-send --action=id=Label --wait TITLE BODY)
-- >   case "$k" in 'id') <callback>;; esac ) </dev/null >/dev/null 2>&1 &
--
-- @--wait@ blocks until the notification closes and prints the clicked
-- action; backgrounding keeps that from stalling the timer that called us.
pipeline :: String -> String -> [(Button, String)] -> String
pipeline title body bcs =
  "( k=$(notify-send " ++ concatMap (action . fst) bcs ++ "--wait "
    ++ shq title ++ " " ++ shq body ++ "); "
    ++ "case \"$k\" in " ++ concatMap arm bcs
    ++ "esac ) </dev/null >/dev/null 2>&1 &"
  where
    action b = "--action=" ++ shq (btnId b ++ "=" ++ btnLabel b) ++ " "
    arm (b, cb) = shq (btnId b) ++ ") " ++ cb ++ ";; "


--------------------------------------------------------------------------------
-- 8. MAIN
--------------------------------------------------------------------------------

-- | Dispatch @check | list | interval | run <name> [<handler>]@.
-- The program path used in callbacks can be overridden with @SCHED_PROG@;
-- otherwise the executable is assumed to be named @remindd@ (what Nix
-- installs, so it is on @PATH@).
defaultMain :: Schedule a -> IO ()
defaultMain sched = do
  args <- getArgs
  prog <- fmap (fromMaybe "remindd") (lookupEnv "SCHED_PROG")
  let rs = collect sched
  case args of
    ["interval"] -> interval rs
    _ -> do
      dir <- stateDir
      case args of
        ["check"] -> check dir prog rs
        ["list"] -> list dir rs
        ["run", name] -> runCmd dir prog rs name Nothing
        ["run", name, h] -> runCmd dir prog rs name (Just h)
        _ -> die "usage: remindd check | list | interval | run <name> [<handler>]"

-- | The state directory: @$XDG_STATE_HOME/sched@, or @~/.local/state/sched@
-- when that is unset.  The store is the file @<dir>/state@.
stateDir :: IO FilePath
stateDir = do
  mx <- lookupEnv "XDG_STATE_HOME"
  case mx of
    Just d | not (null d) -> return (d ++ "/sched")
    _ -> fmap (++ "/.local/state/sched") getHomeDirectory
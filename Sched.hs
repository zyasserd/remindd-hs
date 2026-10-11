-- | A tiny scheduler DSL for microhs.
--
-- @Sched@ is a State monad over the persistent 'Store', layered on a
-- read-only environment (the time, plus the active state-key prefix) and IO.
-- A reminder is just a @Sched ()@ program.
--
-- The whole schedule is one @Sched ()@.  Wrap each reminder in 'namespace'
-- to give it its own state namespace: keys become @\"stretch.lastDone\"@,
-- but the prefix is handled for you — you only ever write plain keys.
--
-- microhs has no @Control.Monad.State@, but ships a @StateT@ in
-- @Data.Foldable.Internal@, which we reuse.  microhs also cannot resolve a
-- class constraint on an /inline/ overloaded expression (e.g. @return ()@, a
-- pure @0@); pass a named function or annotate the value.
module Sched
  ( -- * The monad
    Sched
  , getS
  , setS
  , now
  , liftS
    -- * Shell
  , cmd
  , readIntCmd
    -- * Reminders
  , namespace
  , every
  , everyWith
  , condition
    -- * Time units
  , sec
  , min
  , hr
  , day
  , week
    -- * Running
  , defaultMain
  , runSchedulerIn
  ) where

import System.Process (system, readProcess)
import Control.Monad (when, void)
import Data.Foldable.Internal (StateT(..))
import System.Exit (ExitCode(..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  )
import System.Environment (lookupEnv)
import System.IO.TimeMilli (getTimeMilli)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | Persistent key/value state.  A fresh key reads as 0.
type Store = [(String, Int)]

-- Time units (in seconds) -------------------------------------------------

sec, min, hr, day, week :: Int
sec = 1
min = 60
hr = 60 * 60
day = 24 * hr
week = 7 * day

-- State monad transformer helper

liftState :: Monad m => m a -> StateT s m a
liftState m = StateT (\s -> do
  a <- m
  return (s, a))

-- Effect monad: 'Clock' is equiv to @ReaderT Env IO@ — the Reader monad over IO, with
-- a read-only environment holding the time and the active key prefix.
data Env = Env { envNow :: Int, envPrefix :: String }

newtype Clock a = Clock { runClock :: Env -> IO a }

instance Functor Clock where
  fmap f (Clock g) = Clock (\e -> fmap f (g e))

instance Applicative Clock where
  pure a = Clock (\_ -> return a)
  Clock f <*> Clock g = Clock (\e -> f e <*> g e)

instance Monad Clock where
  return = pure
  Clock g >>= k = Clock (\e -> do
    a <- g e
    runClock (k a) e)

askEnv :: Clock Env
askEnv = Clock (\e -> return e)

-- The scheduler monad --------------------------------------------------

-- | A reminder program: state over 'Store', running in 'Clock'.
type Sched = StateT Store Clock

-- | Run a program with an explicit time and store.
runSched :: Int -> Store -> Sched a -> IO (a, Store)
runSched t store p = do
  (store', a) <- runClock (runStateT p store) (Env t "")
  return (a, store')

-- | The current time in Unix seconds (fixed for the whole run).
now :: Sched Int
now = liftState (fmap envNow askEnv)

-- | Lift an 'IO' action into 'Sched'.
liftS :: IO a -> Sched a
liftS = liftState . Clock . const

prefix :: Sched String
prefix = liftState (fmap envPrefix askEnv)

-- | Read a persistent value (0 if unset).  Keys are scoped by the enclosing
-- 'namespace'.
getS :: String -> Sched Int
getS k = do
  p <- prefix
  StateT (\s -> return (s, fromMaybe 0 (lookup (p ++ k) s)))

-- | Write a persistent value (scoped by the enclosing 'namespace').
setS :: String -> Int -> Sched ()
setS k v = do
  p <- prefix
  StateT (\s -> return (setI (p ++ k) v s, ()))
  where
    setI :: String -> Int -> Store -> Store
    setI key val st = (key, val) : filter ((/= key) . fst) st

-- | Run a shell command and return its exit status (0 = success).
cmd :: String -> Sched Int
cmd c = liftS (fmap exitCode (system c))
  where
    exitCode :: ExitCode -> Int
    exitCode ExitSuccess = 0
    exitCode (ExitFailure n) = n

-- | Run a shell command and parse its stdout as an integer, or 'Nothing' if
-- there is none.  Use this for external last-done sources (e.g. the mtime of
-- a file, which is absent until the file exists).  A failing command yields
-- 'Nothing' rather than an error.
readIntCmd :: String -> Sched (Maybe Int)
readIntCmd c = liftS (fmap readMaybe (capture c))
  where
    -- @|| true@ so a nonzero exit is not an error ('readProcess' would throw
    -- otherwise); the parens keep the redirect attached to the whole command.
    capture :: String -> IO String
    capture cmd = readProcess ("(" ++ cmd ++ " || true) 2>/dev/null") [] ""

-- Reminders ------------------------------------------------------------

-- | Run a program in its own state namespace.  Keys written inside become
-- @name.key@, so reminders can each use @\"lastDone\"@ etc. without
-- clashing.  The prefix is invisible to the program.
namespace :: String -> Sched a -> Sched a
namespace name p = StateT (\s -> Clock (\e ->
  runClock (runStateT p s) e { envPrefix = envPrefix e ++ name ++ "." }))

-- | Run the action when more than @secs@ seconds have passed since it last
-- ran.  The last-run time is persisted, so it will not fire early across
-- runs.
every :: Int -> Sched a -> Sched ()
every secs act = do
  t <- now
  ld <- getS "lastDone"
  when (t > ld + secs) $ do
    setS "lastDone" t
    void act

-- | Like 'every', but the last-done time also comes from an external source
-- ('Nothing' if it is unknown).  The more recent of the two wins, so the
-- action will not fire early.
everyWith :: Int -> Sched (Maybe Int) -> Sched a -> Sched ()
everyWith secs ld act = do
  ml <- ld
  ld' <- getS "lastDone"
  setS "lastDone" (maybe ld' (max ld') ml)
  every secs act

-- | Check at most once per @secs@.  A true result grows a persisted streak,
-- a false result resets it.  When the streak reaches @trigger@ the action
-- runs and the streak resets.  Built on 'every'.
condition :: Int -> Int -> Sched Bool -> Sched a -> Sched ()
condition secs trigger chk act =
  every secs $ do
    ok <- chk
    streak <- getS "streak"
    let streak' = if ok then streak + 1 else 0
    setS "streak" streak'
    when (streak' >= trigger) $ do
      setS "streak" 0
      void act

-- Running --------------------------------------------------------------

-- | Run a schedule with the default state directory.
defaultMain :: Sched () -> IO ()
defaultMain p = do
  dir <- stateDir
  runSchedulerIn dir p
  where
    stateDir :: IO FilePath
    stateDir = do
      mx <- lookupEnv "XDG_STATE_HOME"
      case mx of
        Just d | not (null d) -> return (d ++ "/sched")
        _ -> do
          h <- getHomeDirectory
          return (h ++ "/.local/state/sched")

-- | Run a schedule, persisting state under @dir@.
runSchedulerIn :: FilePath -> Sched () -> IO ()
runSchedulerIn dir p = do
  createDirectoryIfMissing True dir
  t <- nowSeconds
  store <- readStore path
  (_, st) <- runSched t store p
  writeStore path st
  where
    path :: FilePath
    path = dir ++ "/state"
    nowSeconds :: IO Int
    nowSeconds = do
      ms <- getTimeMilli
      return (ms `div` 1000)

-- Store I/O ------------------------------------------------------------

readStore :: FilePath -> IO Store
readStore path = do
  ex <- doesFileExist path
  if ex
    then do
      s <- readFile path
      return (parseStore s)
    else return []

parseStore :: String -> Store
parseStore = concatMap parseLine . lines
  where
    parseLine :: String -> Store
    parseLine l = case words l of
      [k, v] -> [(k, readInt v)]
      _ -> []

writeStore :: FilePath -> Store -> IO ()
writeStore path st = writeFile path (unlines (map showPair st))
  where
    showPair (k, v) = k ++ " " ++ show v

readInt :: String -> Int
readInt = fromMaybe 0 . readMaybe

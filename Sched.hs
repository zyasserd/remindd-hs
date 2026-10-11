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
-- microhs has no @Control.Monad.State@, so the monad is defined here.  It
-- also cannot resolve a class constraint on an /inline/ overloaded
-- expression (e.g. @return ()@, a pure @0@); pass a named function or
-- annotate the value.
module Sched
  ( -- * The monad
    Sched
  , getS
  , setS
  , now
  , liftS
    -- * Shell
  , cmd
  , parseFirstIntCmd
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

import System.Process (system)
import System.Exit (ExitCode(..))
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  , getTemporaryDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.IO.TimeMilli (getTimeMilli)
import Data.Char (isDigit, digitToInt)
import Data.List (lookup)

-- | Persistent key/value state.  A fresh key reads as 0.
type Store = [(String, Int)]

-- Time units (in seconds) -------------------------------------------------

sec, min, hr, day, week :: Int
sec = 1
min = 60
hr = 60 * 60
day = 24 * hr
week = 7 * day

-- State monad transformer (microhs has no Control.Monad.State) --------

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (\s -> do
    (a, s') <- g s
    return (f a, s'))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  StateT f <*> StateT g = StateT (\s -> do
    (h, s1) <- f s
    (a, s2) <- g s1
    return (h a, s2))

instance Monad m => Monad (StateT s m) where
  return = pure
  StateT g >>= k = StateT (\s -> do
    (a, s1) <- g s
    runStateT (k a) s1)

liftState :: Monad m => m a -> StateT s m a
liftState m = StateT (\s -> do
  a <- m
  return (a, s))

-- Effect monad: read-only environment (time + key prefix) over IO -----

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
runSched t store p = runClock (runStateT p store) (Env t "")

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
  StateT (\s -> return (getI (p ++ k) s, s))
  where
    getI :: String -> Store -> Int
    getI key st = case lookup key st of
      Just v -> v
      Nothing -> 0

-- | Write a persistent value (scoped by the enclosing 'namespace').
setS :: String -> Int -> Sched ()
setS k v = do
  p <- prefix
  StateT (\s -> return ((), setI (p ++ k) v s))
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

-- | Run a shell command and parse the first integer in its stdout.  Use this
-- for external last-done sources (e.g. the mtime of a file).
-- TODO: figure out how to simplify
parseFirstIntCmd :: String -> Sched Int
parseFirstIntCmd c = liftS (fmap parseFirstInt (capture c))
  where
    capture :: String -> IO String
    capture c = do
      tmp <- getTemporaryDirectory
      t <- getTimeMilli
      let out = tmp ++ "/sched_capture_" ++ show t ++ ".out"
      _ <- system ("(" ++ c ++ ") > " ++ out ++ " 2>/dev/null")
      ex <- doesFileExist out
      if not ex
        then return ""
        else do
          s <- readFile out
          s' <- seq (length s) (return s)
          removeFile out
          return s'

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
  if t > ld + secs
    then do
      setS "lastDone" t
      _ <- act
      return ()
    else return ()

-- | Like 'every', but the last-done time also comes from an external source.
-- The more recent of the two wins, so the action will not fire early.
everyWith :: Int -> Sched Int -> Sched a -> Sched ()
everyWith secs ld act = do
  l <- ld
  ld' <- getS "lastDone"
  if l > ld' then setS "lastDone" l else return ()
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
    if streak' >= trigger
      then do
        setS "streak" 0
        _ <- act
        return ()
      else return ()

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
      [k, v] -> [(k, parseFirstInt v)]
      _ -> []

writeStore :: FilePath -> Store -> IO ()
writeStore path st = writeFile path (unlines (map showPair st))
  where
    showPair (k, v) = k ++ " " ++ show v

parseFirstInt :: String -> Int
parseFirstInt s =
  case dropWhile (not . isDigit) s of
    [] -> 0
    s' -> let (ds, _) = span isDigit s'
          in foldl (\acc c -> acc * 10 + digitToInt c) 0 ds

module Sched
  ( Reminder(..)
  , Action(..)
  , Check(..)
  , LastDone(..)
  , Store
  , interval
  , intervalWith
  , intervalWithConst
  , condition
  , machine
  , rName
  , defaultMain
  , runScheduler
  , runSchedulerIn
  , defaultStateDir
  , currentSeconds
  , shCheck
  , shTime
  , runSh
  , getI
  , setI
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
import Data.Char (isDigit, isAlpha, digitToInt)
import Data.List (lookup)

-- | What to do when a reminder fires.  The DSL has instances for a plain
-- @IO ()@ action and for a shell command 'String'.
--
--   NOTE (microhs): pass a /named/ function or annotate the value, e.g.
--   @(return () :: IO ())@.  microhs cannot resolve a class constraint on an
--   overloaded expression such as @return ()@ or @pure ()@ written inline.
class Action a where
  toAction :: a -> IO ()

instance Action (IO ()) where
  toAction = id

instance Action String where
  toAction = runSh

-- | The predicate of a condition reminder: @IO Bool@ or a shell command
-- whose exit status is the answer (0 = true).
class Check a where
  toCheck :: a -> IO Bool

instance Check (IO Bool) where
  toCheck = id

instance Check String where
  toCheck = shCheck

-- | The \"last done\" reference of an interval reminder: @IO Int@, a
-- constant 'Int', or a shell command whose stdout is Unix seconds.
class LastDone a where
  toLastDone :: a -> IO Int

instance LastDone (IO Int) where
  toLastDone = id

instance LastDone Int where
  toLastDone n = return n

instance LastDone String where
  toLastDone cmd = do
    out <- shTime cmd
    return out

-- | A reminder is a small state machine evaluated once per run.
--
--   * 'Interval': due when @now > lastDone + every@.  @lastDone@ comes from
--     the override (if given) or from the persisted state.
--   * 'Condition': the check runs at most once per @every@; a true result
--     grows a counter, a false result resets it.  When the counter reaches
--     @trigger@ the action runs and the counter resets.
--   * 'Machine': any state machine; it gets the clock and the persisted
--     'Store' and returns the new store plus an optional action to run.
data Reminder
  = Interval String Int (Maybe (IO Int)) (IO ())
  | Condition String Int Int (IO Bool) (IO ())
  | Machine String (Int -> Store -> IO (Store, Maybe (IO ())))

interval :: Action a => String -> Int -> a -> Reminder
interval name every act = Interval name every Nothing (toAction act)

intervalWith :: (LastDone l, Action a) => String -> Int -> l -> a -> Reminder
intervalWith name every ld act = Interval name every (Just (toLastDone ld)) (toAction act)

-- | Interval with a concrete last-done time.  Use this with inline literals
-- (e.g. @intervalWithConst "x" 60 0 act@) which microhs cannot pass through
-- the 'LastDone' class.
intervalWithConst :: Action a => String -> Int -> Int -> a -> Reminder
intervalWithConst name every ld act = Interval name every (Just (return ld)) (toAction act)

condition :: (Check c, Action a) => String -> Int -> Int -> c -> a -> Reminder
condition name every trigger chk act =
  Condition name every trigger (toCheck chk) (toAction act)

machine :: String -> (Int -> Store -> IO (Store, Maybe (IO ()))) -> Reminder
machine = Machine

rName :: Reminder -> String
rName (Interval n _ _ _) = n
rName (Condition n _ _ _ _) = n
rName (Machine n _) = n

-- | Persistent per-reminder key/value state.
type Store = [(String, Int)]

getI :: String -> Store -> Int
getI k st = case lookup k st of
  Just v -> v
  Nothing -> 0

setI :: String -> Int -> Store -> Store
setI k v st = (k, v) : filter ((/= k) . fst) st

currentSeconds :: IO Int
currentSeconds = do
  ms <- getTimeMilli
  return (ms `div` 1000)

defaultStateDir :: IO FilePath
defaultStateDir = do
  mx <- lookupEnv "XDG_STATE_HOME"
  case mx of
    Just d | not (null d) -> return (d ++ "/sched")
    _ -> do
      h <- getHomeDirectory
      return (h ++ "/.local/state/sched")

defaultMain :: [Reminder] -> IO ()
defaultMain rs = do
  dir <- defaultStateDir
  runSchedulerIn dir rs

runScheduler :: [Reminder] -> IO ()
runScheduler = defaultMain

runSchedulerIn :: FilePath -> [Reminder] -> IO ()
runSchedulerIn dir rs = do
  createDirectoryIfMissing True dir
  now <- currentSeconds
  mapM_ (runOne dir now) rs

runOne :: FilePath -> Int -> Reminder -> IO ()
runOne dir now r = do
  let path = dir ++ "/" ++ stateName (rName r) ++ ".state"
  st <- readStore path
  pair <- evaluate now r st
  let st' = fst pair
      mact = snd pair
  writeStore path st'
  case mact of
    Just act -> act
    Nothing -> return ()

evaluate :: Int -> Reminder -> Store -> IO (Store, Maybe (IO ()))
evaluate now (Interval _ every mld act) st = do
  lastDone <- case mld of
    Just f -> f
    Nothing -> return (getI "lastDone" st)
  if now > lastDone + every
    then return (setI "lastDone" now st, Just act)
    else return (st, Nothing)
evaluate now (Condition _ every trigger chk act) st = do
  let lastCheck = getI "lastCheckAt" st
  if lastCheck /= 0 && now - lastCheck < every
    then return (st, Nothing)
    else do
      ok <- chk
      let streak = getI "streak" st
          streak' = if ok then streak + 1 else 0
          st1 = setI "streak" streak' (setI "lastCheckAt" now st)
      if streak' >= trigger
        then return (setI "streak" 0 st1, Just act)
        else return (st1, Nothing)
evaluate now (Machine _ f) st = f now st

-- | Run a shell command as an action, ignoring its exit status so one
-- failing reminder does not stop the rest of the run.
runSh :: String -> IO ()
runSh cmd = do
  _ <- system cmd
  return ()

-- | Run a shell command, returning @True@ iff its exit status is 0.
shCheck :: String -> IO Bool
shCheck cmd = do
  ec <- system cmd
  return (ec == ExitSuccess)

-- | Run a shell command and parse the first integer in its stdout (Unix
-- seconds for @lastDoneOverride@).  Returns 0 if there is no number.
shTime :: String -> IO Int
shTime cmd = do
  out <- capture cmd
  return (parseFirstInt out)

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
    parseLine l = case words l of
      [k, v] -> [(k, readInt v)]
      _ -> []

writeStore :: FilePath -> Store -> IO ()
writeStore path st = writeFile path (unlines (map showPair st))
  where
    showPair (k, v) = k ++ " " ++ show v

capture :: String -> IO String
capture cmd = do
  tmp <- getTemporaryDirectory
  t <- getTimeMilli
  let out = tmp ++ "/sched_capture_" ++ show t ++ ".out"
  _ <- system (cmd ++ " > " ++ out ++ " 2>/dev/null")
  ex <- doesFileExist out
  if not ex
    then return ""
    else do
      s <- readFile out
      s' <- seq (length s) (return s)
      removeFile out
      return s'

parseFirstInt :: String -> Int
parseFirstInt s =
  case dropWhile (not . isDigit) s of
    [] -> 0
    s' -> let (ds, _) = span isDigit s'
          in foldl (\acc c -> acc * 10 + digitToInt c) 0 ds

readInt :: String -> Int
readInt = parseFirstInt

stateName :: String -> String
stateName = filter ok
  where
    ok c = isDigit c || isAlpha c || c == '-' || c == '_'

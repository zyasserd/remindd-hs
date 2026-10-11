-- | Your schedule, written as a Haskell DSL.
--
-- Run it with microhs:
--
--     mhs -r a.hs
--
-- or build a binary (needs a C compiler):
--
--     mhs a.hs -o sched
--
-- The @Sched@ module provides the types, classes and the runner; here you
-- only describe /what/ to do.  State lives in
-- @$XDG_STATE_HOME/sched/<name>.state@ (fallback @~/.local/state/sched@).
--
-- microhs note: pass /named/ functions or annotated values to the smart
-- constructors.  Inline overloaded expressions such as @return ()@ or
-- @pure True@ cannot be resolved by microhs and must be written
-- @(return () :: IO ())@ etc.

module Main (main) where

import Sched
import System.Process (callCommand)

-- Actions ---------------------------------------------------------------
-- Either an @IO ()@ function or a shell command 'String'.

stretchAction :: IO ()
stretchAction = callCommand "notify-send 'Stretch' 'Time to move'"

backupAction :: IO ()
backupAction = callCommand "notify-send 'Backup' 'Run your backups'"

powerAction :: IO ()
powerAction = callCommand "notify-send 'Power saver' 'Still on power-saver'"

-- Conditions ------------------------------------------------------------
-- Either an @IO Bool@ function or a shell command (exit 0 = true).

powerCheck :: IO Bool
powerCheck = shCheck "powerprofilesctl get | grep -vq power-saver"

-- Last-done sources -----------------------------------------------------
-- Either an @IO Int@ function, a constant 'Int', or a shell command whose
-- stdout is Unix seconds.

flakeTime :: IO Int
flakeTime = shTime "stat -c %Y /etc/nixos/flake.lock 2>/dev/null || echo 0"

-- The schedule ----------------------------------------------------------

reminders :: [Reminder]
reminders =
  [ -- Every 2h, unless the lock file was already touched more recently.
    -- The override command's stdout is used as the last-done time, so the
    -- reminder will not fire early if the action ran in between.
    intervalWith "stretch" 7200 flakeTime stretchAction

    -- Check every 30m.  After 12 consecutive trues the action fires.
    -- The true-streak is persisted, so it accumulates across runs.
  , condition "powerSaver" 1800 12 powerCheck powerAction

    -- A plain interval: run the action whenever 24h have passed.
  , interval "dailyBackup" 86400 backupAction

    -- A custom state machine: fire every third time it is evaluated.
  , machine "everyThird" everyThird
  ]

everyThird :: Int -> Store -> IO (Store, Maybe (IO ()))
everyThird _ st =
  let n = getI "count" st + 1
  in if n >= 3
       then return (setI "count" 0 st, Just (callCommand "notify-send 'Third' 'Tick'"))
       else return (setI "count" n st, Nothing)

main :: IO ()
main = defaultMain reminders

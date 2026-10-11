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
-- A reminder is a @Sched ()@ program.  'getS' / 'setS' read and write
-- persisted state, 'now' is the clock, 'cmd' runs a shell command (returning
-- its exit status, 0 = success), and 'parseFirstIntCmd' captures a command's
-- stdout as a number (for external last-done sources).  'every' and
-- 'condition' are just monadic functions, so a reminder can also be plain
-- @do@ notation.
--
-- 'namespace' gives each reminder its own state namespace, so the
-- @\"count\"@ below and the combinators' @\"lastDone\"@ never clash.
--
-- State lives in @$XDG_STATE_HOME/sched/state@ (fallback
-- @~/.local/state/sched@).

module Main (main) where

import Prelude hiding (min)
import Sched

schedule :: Sched ()
schedule = do
  -- Every 2h, unless the lock file was touched more recently (so it will not
  -- fire early if the action ran in between).
  namespace "stretch" $
    everyWith (2 * hr) flakeTime $
      cmd "notify-send 'Stretch' 'Time to move'"

  -- Check every 30m; after 12 consecutive trues the action fires.  The
  -- true-streak is persisted, so it accumulates across runs.
  namespace "powerSaver" $
    condition (30 * min) 12 powerCheck $
      cmd "notify-send 'Power saver' 'Still on power-saver'"

  -- Every 24h.
  namespace "dailyBackup" $
    every day $
      cmd "notify-send 'Backup' 'Run your backups'"

  -- Custom logic: fire every third time.  Ordinary @do@ notation is enough.
  namespace "everyThird" $ do
    n <- getS "count"
    let n' = n + 1
    if n' >= 3
      then do
        setS "count" 0
        _ <- cmd "notify-send 'Third' 'Tick'"
        return ()
      else setS "count" n'
  where
    -- External last-done source: the mtime of the flake lock file.
    flakeTime :: Sched Int
    flakeTime = parseFirstIntCmd "stat -c %Y /etc/nixos/flake.lock 2>/dev/null || echo 0"
    powerCheck :: Sched Bool
    powerCheck = fmap (== 0) (cmd "powerprofilesctl get | grep -vq power-saver")

main :: IO ()
main = defaultMain schedule

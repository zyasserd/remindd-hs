-- | An example schedule: what to put in @services.remindd.program@.  It is
-- just documentation for this repository; nothing in Nix reads it.
--
-- To try it standalone: @mhs example.hs -o remindd@ (or @ghc@), then run
-- @remindd check@ from a timer or @remindd list@ by hand.

import Lib

--------------------------------------------------------------------------------
-- Guards and actions: ordinary values in 'Act'.
--------------------------------------------------------------------------------

-- | True when the file named by @cmd@ was last modified more than @period@
-- seconds ago.  This is remindd's @lastDoneOverride@, typed by hand: the
-- interval is measured from an external clock (a file's mtime) rather than
-- from the last time this reminder fired.
overdueFor :: Int -> String -> Act Bool
overdueFor period cmd = do
  t <- now
  m <- readIntCmd cmd
  return (t - (case m of Just x -> x; Nothing -> 0) > period)

-- | True while the system is /not/ on power-saver.
powerSaverOff :: Act Bool
powerSaverOff = succeeds "powerprofilesctl get | grep -vq power-saver"

--------------------------------------------------------------------------------
-- The schedule
--------------------------------------------------------------------------------

schedule :: Schedule ()
schedule = do

  -- interval every 3 weeks, keyed off flake.lock's mtime: nags once a day
  -- while the lock is stale.  (remindd: lastDoneOverride, snooze 86400)
  remind "osUpdate" day $ do
    provided (overdueFor (3 * week) "stat -c %Y /etc/nixos/flake.lock")
    notify "Update NixOS" "flake.lock is over 3 weeks old"
      [ button "update" "Update" (shell "code /etc/nixos")
      , button "snooze" "Snooze" (snooze day) ]

  -- condition every 10 min, fires after 3 in a row (30 min); snooze 2h.
  remind "powerSaver" (10 * minute) $ do
    provided (sustained 3 powerSaverOff)
    notify "Activate Power Saver Mode" "Power saver has been off for 30m"
      [ button "activate" "Activate" (shell "powerprofilesctl set power-saver")
      , button "snooze" "Snooze 2h" (snooze (2 * hour)) ]

  -- interval every 3 weeks.
  remind "backupSSD" (3 * week) $
    notify "SSD Backup" "Time to back up the SSD"
      [ button "done" "Mark Done & Open Kopia"
          (shell "xdg-open 'http://kopia.local'")
      , button "snooze" "Snooze" (snooze day) ]

  -- interval every week.
  remind "backupFF" week $
    notify "FolderFort Backup" "Time for a FolderFort backup"
      [ button "done" "Mark Done & Open Kopia"
          (shell "xdg-open 'http://kopia.local'")
      , button "snooze" "Snooze" (snooze day) ]

main :: IO ()
main = defaultMain schedule

-- | Example schedule.  Build with: mhs -i. Example.hs -o sched
-- then run @sched check@ from cron or a systemd timer.

import Lib

--------------------------------------------------------------------------------
-- Your own typed state: any Show/Read type, declared once as a value.
--------------------------------------------------------------------------------

walks :: Var Int
walks = var "walks" 0

--------------------------------------------------------------------------------
-- Your own guards and actions: ordinary values in 'Act'.
--------------------------------------------------------------------------------

lowBattery :: Act Bool
lowBattery = do
  pct <- readIntCmd "cat /sys/class/power_supply/BAT0/capacity"
  return (maybe False (< 20) pct)

workHours :: Act Bool
workHours = do
  h <- readIntCmd "date +%-H"
  return (maybe False (\x -> x >= 9 && x < 18) h)

--------------------------------------------------------------------------------
-- The schedule
--------------------------------------------------------------------------------

schedule :: Schedule ()
schedule = do

  -- Simplest: periodic notification with a snooze button.
  remind "stretch" (2 * hours) $
    notify "Stretch" "Time to move"
      [ button "snooze" "Snooze 1h" (snooze hours) ]

  -- Guard on a condition about the world.
  remind "water" (45 * minutes) $ do
    provided workHours
    notify "Water" "Drink something" []

  -- Debounced: only fires after 3 low readings in a row (15 minutes).
  remind "battery" (5 * minutes) $ do
    provided (sustained 3 lowBattery)
    notify "Battery" "Under 20% - plug in"
      [ button "later" "Remind in 30m" (snooze (30 * minutes)) ]
    onFire (shell "logger 'battery low'")

  -- Typed state in a guard and an effect: nags at most 3 times.
  remind "walk" hours $ do
    provided (fmap (< 3) (get walks))
    notify "Walk" "Take a short walk" []
    onFire (modify walks (+ 1))

main :: IO ()
main = defaultMain schedule
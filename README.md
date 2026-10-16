# remindd-hs

A tiny, daemonless desktop reminder scheduler, written in Haskell for
microhs.  A reminder is a **period**, some **guards** and some **effects**:

```haskell
remind "battery" (5 * minute) $ do
  provided (sustained 3 lowBattery)      -- WHEN
  notify "Battery" "Plug in" []          -- WHAT
```

Run `remindd check` from an external timer (cron, systemd); `remindd list`,
`remindd run <name>` (acknowledge) and `remindd reset <name>` are for by hand.
State lives in `$XDG_STATE_HOME/remindd/state` (fallback
`~/.local/state/remindd/state`).

## Install (Home Manager)

The flake exposes the module as `homeManagerModules.remindd`:

```nix
# flake.nix inputs
inputs.remindd-hs.url = "github:zyasserd/remindd-hs";

# home.nix
imports = [ inputs.remindd-hs.homeManagerModules.remindd ];

services.remindd = {
  enable = true;
  source = ./remindd;   # a directory; it must contain main.hs
};
```

The schedule lives in `main.hs` inside that directory:

```haskell
import Lib

schedule :: Schedule ()
schedule =
  remind "water" (45 * minute) $
    notify "Water" "Drink something" []

main :: IO ()
main = defaultMain schedule
```

The module compiles `main.hs` with microhs into the `remindd` command and
installs it next to `libnotify`.  Other `*.hs` files are compiled as modules
`main.hs` can import (`Lib.hs` is reserved — `Lib` comes from the module).  Any
*non-Haskell* file in the directory is installed in `libexec/remindd`,
**private to the program** — it never appears on `PATH`.  The schedule reaches
it with `Lib.bundled`, e.g.:

```haskell
runBackup = do
  cmd <- bundled "backup-kopia-hetzner.sh"
  ok <- logCmd cmd
  ...
```

It runs `remindd check` on a systemd **user timer** (`remindd-check`).  The
timer's period is `remindd interval` — the shortest reminder period — read from
the built schedule at evaluation time, so it behaves like remindd's derived
interval.

## Build a standalone binary

```
mhs example.hs -o remindd      # microhs (needs a C compiler)
ghc example.hs -o remindd      # GHC (Lib.hs alongside; e.g. `nix-shell -p ghc`)
```

`Lib.hs` is a single source for both compilers: it avoids microhs-only
modules (`readProcess`, `getTimeMilli`) by shelling out with `system` and a
temp file.  Under GHC the only non-`base` dependency is `process`, which ships
with GHC.

## CLI

| command                     | meaning                                          |
|-----------------------------|--------------------------------------------------|
| `remindd check`               | evaluate due reminders; send notifications       |
| `remindd list`                | per reminder: a **check** group (every, last, next) and an **act** group (fired, done), plus summary |
| `remindd interval`            | shortest period, in seconds (for a timer)        |
| `remindd run <name>`          | acknowledge a reminder (mark it done now)        |
| `remindd run <name> <button>` | run one of its notification buttons              |
| `remindd reset <name>`        | forget a reminder's state, so it is due again    |

`list` has two groups of columns, because a reminder has two clocks.  **check**
is the scheduler's side: `every` (its period), `last` (when its guards last
ran) and `next` (`due` / `in 5m` / `snoozed 2h` — when it will be looked at
again).  **act** is what has actually happened: `fired` (when its effects last
ran — the program notified or executed) and `done` (when *you* last
acknowledged).  Firing does not acknowledge, so the two move independently: a
reminder can fire many times without ever being done — that is what nagging
means — and `done` can happen before it ever fires.  A guard's outcome is never
stored, so it is not shown; only a `summary` can reveal it, and a `summary` is
also where you predict when a guard will next hold (e.g. `lastDone + period`),
since `next` only says when the scheduler will next *look*.  The `summary`
column appears only once some reminder defines one.

## Writing a schedule

`schedule :: Schedule ()` is one `do` block:

- `remind name period cfg` declares a reminder, evaluated at most once per
  `period` seconds (units: `second`, `minute`, `hour`, `day`, `week`).
- `provided (Act Bool)` gates it — guards run in order, the first false one
  stops it.  `sustained n test` is true only after `n` true readings in a row;
  `overdue n` is true once `n` seconds have passed since the reminder was last
  `done` (so a never-done reminder is overdue at once).
- `onFire (Act ())` adds a side effect.
- `handler name (Act ())` registers a button action, run by
  `remindd run <name> <handler>`.
- `done` marks the reminder done now (clears its snooze and streak); `snooze n`
  skips evaluation for `n` seconds.
- `var "slot" default :: Var a` declares typed state; `get`/`set`/`modify`
  read and write it.  A reminder's state is isolated from every other's.
- `summary (View String)` adds a one-line description to `remindd list`, e.g.
  `summary (do s <- get' streak; return ("streak " ++ show s ++ " of 3"))`.
  Since `list` never runs guards, this is also where you predict act timing:
  `dueIn period` (companion to `overdue`) reads `"due in 2w"` from
  `lastDone + period`, and `dueAt instant` renders any deadline you computed.
  `View` is a pure read of the reminder's state (`get'`, `now'`, and the core
  `Var`s `lastDone`, `streak`, …), so listing can never notify or change
  anything.  `humanize` and `ago` render durations (`3w`, `2h ago`).  The
  `summary` column only appears once some reminder defines one.  To describe
  something a guard measures live (a file's age, a command's output), have the
  guard `set` it into a `Var` first and read that back in the summary.

**Notifying is not doing.**  A reminder is evaluated once per `period`; while
its guards hold it notifies every time, so it keeps nagging until an action
calls `done` (or you `snooze` it).  Firing changes no schedule state — only
`done` (via a button or `remindd run <name>`) does.

### Effects (`Act a`)

`io`, `now`, `shell`, `succeeds`, `readIntCmd`, `lastModified`, `bundled`,
`callback`, `logLine`, `logCmd`, plus the core state actions.  Ordinary `do`
notation; anything `IO` lifts with `io`.

### Notifications (extension)

`notify title body [button ...]` sends a `notify-send` notification with
buttons.  `button id label (Act ())` adds one; clicking it runs the action.
`snooze seconds` silences a reminder for a while.  Notifications are sent by a
`notify-send --wait` kept alive in the background, and carry
`--app-name=remindd` plus a `desktop-entry` hint.  Together with the
`remindd.desktop` the module registers via `xdg.desktopEntries`, GNOME groups
every reminder under a single **remindd** source.  Both parts matter: without
the desktop entry GNOME keeps one source per pid, and without `--wait` it
destroys the source the instant `notify-send` exits.

`notifyNow title body [button ...]` sends one immediately from an `onFire` or a
`handler`, for when whether to notify is only known at run time — e.g. a backup
that notifies on success or failure (its buttons must be registered with
`handler` first, since `notifyNow` registers nothing).

### Long jobs and logging

A reminder can do real work — a backup over the network.  It is just ordinary
`do` and `if`: the command's exit status is a Boolean, and `logCmd` runs it with
its output appended to the shared log:

```haskell
runBackup :: Act ()
runBackup = do
  ok <- logCmd cmd
  if ok then do done; notifyNow "Backup done" "Uploaded" []
        else notifyNow "Backup failed" "Read the log"
               [ button "retry" "Retry"    runBackup
               , button "logs"  "Open log" openLog
               , button "snz"   "Snooze"   (snooze day) ]

remind "backup" day $ do
  provided (overdue week)
  handler "retry" runBackup          -- notifyNow registers nothing, so a
  handler "logs"  openLog            -- button's id must be a handler
  handler "snz"   (snooze day)
  onFire runBackup
```

A job runs inline, so it holds the state lock for as long as it takes.  The lock
records its holder's pid and process start time, so a second tick **waits**
rather than starting a second copy, and a lock is stolen only when its holder is
no longer running -- never because it is simply old, so a backup that runs for
hours is safe.  `logLine s` appends `TIME [name] s`; `logCmd cmd` brackets the
command with `run`/`exit` lines and prefixes **every** output line as
`TIME [name] | …`, so one shared log never has an unattributed line (the exit
status is captured separately, since the tagging pipeline ends in `awk`).
`logPath` is that file (`<state dir>/log`), one log for every reminder instead
of one per reminder.

## Platform assumptions

"Portable" here means **one source for microhs and GHC**, not cross-OS.  The
program deliberately shells out instead of using FFI, and is Linux/`/bin/sh`
software.  At run time it needs:

- **`/proc`** — the lock identifies its holder by pid *and* the process's start
  time, read from `/proc/<pid>/stat`, and checks liveness with `kill -0`.  Both
  are Linux-only (this is the one place the scheduler is not portable).
- **A shell and coreutils** — `system` runs `/bin/sh -c`; the commands used are
  `date`, `cat`, `awk`, `printf`, `mv`, `rm`, `sleep`, `sync`, `kill`, and the
  `set -C` builtin.  `sync <file>` (store durability) wants coreutils ≥ 8.24;
  older `sync` ignores its arguments and flushes everything, which is still
  correct.
- **A writable `/tmp`** — capturing a command's output, and `logCmd`'s exit
  side channel, go through temporary files there.  They are removed even if the
  command fails, but a `SIGKILL` at the wrong instant can leave one behind;
  they are inert and `remindd`-named.
- **`notify-send`** (`libnotify`) with `--action`/`--wait`, plus a desktop entry
  so GNOME groups a reminder's notifications under one **remindd** source.  The
  example's "Open log"/Kopia buttons use `xdg-open`.
- **systemd user services/timers** for the Home Manager module.
- **XDG paths** — state lives under `$XDG_STATE_HOME` (fallback
  `~/.local/state/remindd`); `$REMINDD_PROG` overrides the program path used in
  button callbacks.

**Exit status is always 0.** `remindd` reports errors through `die`, which
prints to stderr but exits `0` (microhs cannot exit nonzero quietly), so
`remindd check` is not a reliable failure signal for a timer.  Diagnostics go
to stderr and the shared log; a check that cannot take the lock logs
`[remindd] skipped: another instance holds the lock`.

## Design

`Lib.hs` has two layers:

- **Core** (sections 1–7): the scheduler — the builder, state, actions, the
  engine, the outside-world basics and the lock.  It knows nothing about
  desktop notifications; it only exposes two hooks, `onFire` and `handler`.
- **Notify extension** (section 8): built on just those hooks — the template
  for your own (sound, webhook, …).

A reminder is pure data built by a small writer monad (`Build`); running it
is `Act = Env -> Fields -> IO (a, Fields)`, where `Fields` is the current
reminder's own key/value bag.  One pure function (`nextEval`) says when a
reminder is next due.

## microhs notes

- Give every `var` definition a type signature.
- `Prelude` does not re-export `fromMaybe`/`readMaybe`; import them explicitly.
- `mhs example.hs -o remindd` needs a C compiler; the interpreter (`mhs -r example.hs -- …`)
  does not.

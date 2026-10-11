# remindd-hs

A tiny, stateless desktop reminder scheduler, written in Haskell for
microhs.  A reminder is a **period**, some **guards** and some **effects**:

```haskell
remind "battery" (5 * minutes) $ do
  provided (sustained 3 lowBattery)      -- WHEN
  notify "Battery" "Plug in" []          -- WHAT
```

Run `sched check` from an external timer (cron, systemd); `sched list` and
`sched run <name>` are for by hand.  State lives in
`$XDG_STATE_HOME/sched/state` (fallback `~/.local/state/sched/state`).

## Install (Home Manager)

The flake exposes the module as `homeManagerModules.remindd`:

```nix
# flake.nix inputs
inputs.remindd-hs.url = "github:zyasserd/sched";

# home.nix
imports = [ inputs.remindd-hs.homeManagerModules.remindd ];

services.remindd = {
  enable = true;
  program = builtins.readFile ./a.hs;   # or an inline Haskell string
};
```

The module compiles `program` with microhs into the `sched` command, installs
it next to `libnotify`, and runs `sched check` on a systemd **user timer**
(`remindd-check`).  The timer's period is `sched interval` — the shortest
reminder period — read from the built program at evaluation time, so it
behaves like remindd's derived interval.

## Build a standalone binary

```
mhs a.hs -o sched
```

## CLI

| command                     | meaning                                          |
|-----------------------------|--------------------------------------------------|
| `sched check`               | evaluate due reminders; send notifications       |
| `sched list`                | per reminder: next, lastDone, snoozedUntil       |
| `sched interval`            | shortest period, in seconds (for a timer)        |
| `sched run <name>`          | fire a reminder now                              |
| `sched run <name> <button>` | run one of its notification buttons              |

## Writing a schedule

`schedule :: Schedule ()` is one `do` block:

- `remind name period cfg` declares a reminder, evaluated at most once per
  `period` seconds (units: `seconds`, `minutes`, `hours`, `days`, `weeks`).
- `provided (Act Bool)` gates it — guards run in order, the first false one
  stops it.  `sustained n test` is true only after `n` true readings in a row.
- `onFire (Act ())` adds a side effect.
- `handler name (Act ())` registers a button action, run by
  `sched run <name> <name>`.
- `var "slot" default :: Var a` declares typed state; `get`/`set`/`modify`
  read and write it.  A reminder's state is isolated from every other's.

### Effects (`Act a`)

`io`, `now`, `shell`, `succeeds`, `readIntCmd`, `callback`, plus the core
state actions.  Ordinary `do` notation; anything `IO` lifts with `io`.

### Notifications (extension)

`notify title body [button ...]` sends a `notify-send` notification with
buttons.  `button id label (Act ())` adds one; clicking it runs the action.
`snooze seconds` silences a reminder for a while.

## Design

`Lib.hs` has two layers:

- **Core** (sections 1–6): the scheduler.  It knows nothing about desktop
  notifications; it only exposes two hooks, `onFire` and `handler`.
- **Notify extension** (section 7): built on just those hooks — the template
  for your own (sound, webhook, …).

A reminder is pure data built by a small writer monad (`Build`); running it
is `Act = Env -> Fields -> IO (a, Fields)`, where `Fields` is the current
reminder's own key/value bag.  One pure function (`nextEval`) says when a
reminder is next due.

## microhs notes

- Give every `var` definition a type signature.
- `Prelude` does not re-export `fromMaybe`/`readMaybe`; import them explicitly.
- `mhs a.hs -o sched` needs a C compiler; the interpreter (`mhs -r a.hs -- …`)
  does not.

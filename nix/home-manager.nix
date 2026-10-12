{ config, lib, pkgs, mkSched, ... }:

let
  cfg = config.services.remindd;

  checkServiceName = "remindd-check";

  # The shortest reminder period, read from the schedule itself.  This builds
  # `remindd` and runs `remindd interval` during evaluation (import-from-
  # derivation), the same way remindd derived its timer from the smallest
  # `every`.  Forced only when the module is enabled.
  #
  # `remindd` reports errors by exiting 0 (microhs cannot exit nonzero
  # quietly), so an empty schedule shows up as empty output, which we reject
  # here rather than installing a timer with a blank interval.
  timerInterval =
    let
      seconds = lib.removeSuffix "\n" (builtins.readFile (
        pkgs.runCommand "remindd-interval" { } ''
          ${cfg.package}/bin/remindd interval > $out
        ''
      ));
    in
    if seconds == ""
    then throw "services.remindd: the schedule defines no reminders, so there is no interval"
    else seconds;
in
{
  options.services.remindd = {
    enable = lib.mkEnableOption "remindd, the microhs reminder scheduler";

    source = lib.mkOption {
      type = lib.types.path;
      description = ''
        Directory holding the schedule.  It must contain `main.hs`: a Haskell
        module that imports `Lib` and provides `main :: IO ()`.  Other `*.hs`
        files are compiled as modules `main` can import (`Lib.hs` is reserved:
        `Lib` comes from the module itself).  Every non-Haskell regular file is
        installed in `libexec/remindd`, private to the program: the schedule
        reaches it with `Lib.bundled`, so it never appears on `PATH`.
        Executable files keep their executable bit.

        microhs compiles it into the `remindd` command; a compile error fails
        the build.
      '';
      example = lib.literalExpression "./remindd";
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = mkSched { inherit pkgs; dir = cfg.source; };
      defaultText = lib.literalExpression "remindd built from services.remindd.source";
      description = "The compiled `remindd` package to install.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package pkgs.libnotify ];

    # The notifications identify themselves as the `remindd` app (see Lib.hs),
    # so GNOME can group them under one source.  That only works if GNOME can
    # resolve the id to an installed desktop entry -- without it, GNOME keeps a
    # separate source per notify-send pid and never groups.
    xdg.desktopEntries.remindd = {
      name = "remindd";
      comment = "Tiny reminder scheduler";
      exec = "remindd";
      icon = "appointment-soon";
      terminal = false;
      categories = [ "Utility" ];
      noDisplay = true;
    };

    systemd.user.services."${checkServiceName}" = {
      Unit = {
        Description = "remindd: evaluate reminders";
        # Notifications need the graphical session's DBus; without this an
        # early check runs with no bus, notify-send fails silently (its output
        # is discarded), and the reminder is consumed for a whole period.
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${cfg.package}/bin/remindd check";
        Environment = [ "REMINDD_PROG=${cfg.package}/bin/remindd" ];
        # `check` backgrounds a `notify-send --wait` pipeline and exits; the
        # default KillMode=control-group would tear that down with the service,
        # so keep only the main process in scope.
        KillMode = "process";
      };
    };

    systemd.user.timers."${checkServiceName}" = {
      Unit = {
        Description = "remindd: periodic check";
        PartOf = [ "graphical-session.target" ];
      };
      Timer = {
        OnBootSec = "2m";
        OnUnitActiveSec = timerInterval;
        Unit = "${checkServiceName}.service";
        Persistent = true;
      };
      # Tie the timer to the session rather than timers.target, so it only
      # runs while a graphical session (and thus a notification bus) exists.
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}

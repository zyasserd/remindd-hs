{ config, lib, pkgs, mkSched, defaultProgram ? "", ... }:

let
  cfg = config.services.remindd;

  checkServiceName = "remindd-check";

  # The shortest reminder period, read from the program itself.  This builds
  # `remindd` and runs `remindd interval` during evaluation (import-from-
  # derivation), the same way remindd derived its timer from the smallest
  # `every`.  Forced only when the module is enabled.
  timerInterval =
    lib.removeSuffix "\n" (builtins.readFile (
      pkgs.runCommand "remindd-interval" { } ''
        ${cfg.package}/bin/remindd interval > $out
      ''
    ));
in
{
  options.services.remindd = {
    enable = lib.mkEnableOption "remindd, the microhs reminder scheduler";

    program = lib.mkOption {
      type = lib.types.lines;
      default = defaultProgram;
      description = ''
        The schedule, as Haskell source for a module that imports `Lib` and
        provides `main :: IO ()`.  microhs compiles it into the `remindd`
        command; a compile error fails the build.
      '';
      example = ''
        import Lib

        schedule :: Schedule ()
        schedule =
          remind "water" (45 * minutes) $
            notify "Water" "Drink something" []

        main :: IO ()
        main = defaultMain schedule
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = mkSched { inherit pkgs; program = cfg.program; };
      defaultText = lib.literalExpression "remindd built from services.remindd.program";
      description = "The compiled `remindd` package to install.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package pkgs.libnotify ];

    systemd.user.services."${checkServiceName}" = {
      Unit.Description = "remindd: evaluate reminders";
      Service = {
        Type = "oneshot";
        ExecStart = "${cfg.package}/bin/remindd check";
        Environment = [ "SCHED_PROG=${cfg.package}/bin/remindd" ];
      };
    };

    systemd.user.timers."${checkServiceName}" = {
      Unit.Description = "remindd: periodic check";
      Timer = {
        OnBootSec = "2m";
        OnUnitActiveSec = timerInterval;
        Unit = "${checkServiceName}.service";
        Persistent = true;
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}

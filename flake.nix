{
  description = "remindd-hs: a tiny reminder scheduler written for microhs";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      hmModule = import ./nix/home-manager.nix;

      # Compile one schedule (the `services.remindd.program` source) into the
      # `remindd` command.  `Lib` comes from this flake's source tree.
      mkSched = { pkgs, program }:
        pkgs.stdenv.mkDerivation {
          pname = "remindd";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.microhs pkgs.stdenv.cc ];

          buildPhase = ''
            runHook preBuild
            cp ${pkgs.writeText "schedule.hs" program} schedule.hs
            mhs schedule.hs -o remindd
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            install -Dm755 remindd $out/bin/remindd
            runHook postInstall
          '';
        };
    in
    {
      homeManagerModules.remindd = { pkgs, ... }: {
        imports = [ hmModule ];
        _module.args.mkSched = mkSched;
      };
      homeManagerModules.default = self.homeManagerModules.remindd;
    };
}

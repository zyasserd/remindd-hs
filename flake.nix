{
  description = "remindd-hs: a tiny reminder scheduler written for microhs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    let
      hmModule = import ./nix/home-manager.nix;

      # Compile one schedule (the inline `program` source) into the `remindd`
      # command.  `Lib` comes from this flake's source tree.
      mkSched = { pkgs, program }:
        pkgs.stdenv.mkDerivation {
          pname = "remindd";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = [ pkgs.microhs pkgs.stdenv.cc ];

          buildPhase = ''
            runHook preBuild
            cp ${pkgs.writeText "a.hs" program} a.hs
            mhs a.hs -o remindd
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
        _module.args.defaultProgram = builtins.readFile ./a.hs;
      };
      homeManagerModules.default = self.homeManagerModules.remindd;
    }
    // utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.remindd = mkSched { inherit pkgs; program = builtins.readFile ./a.hs; };
        packages.default = self.packages.${system}.remindd;
      });
}

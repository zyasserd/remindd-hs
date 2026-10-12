{
  description = "remindd-hs: a tiny reminder scheduler written for microhs";

  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      hmModule = import ./nix/home-manager.nix;

      # Compile a schedule directory (see nix/mk-sched.nix) into the `remindd`
      # command.
      mkSched = import ./nix/mk-sched.nix;
    in
    {
      homeManagerModules.remindd = { pkgs, ... }: {
        imports = [ hmModule ];
        _module.args.mkSched = mkSched;
      };
      homeManagerModules.default = self.homeManagerModules.remindd;
    };
}

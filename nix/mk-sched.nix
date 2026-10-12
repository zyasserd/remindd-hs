# Compile a schedule directory into the `remindd` command.
#
# The directory must contain `main.hs`: a module that imports `Lib` and
# provides `main :: IO ()`.  Every other regular file in the directory is
# installed next to `remindd` as an executable, so the schedule can invoke it
# by name (e.g. a `backup-*.sh` helper).  `Lib` comes from this flake's own
# source tree, not from the schedule directory.
{ pkgs, dir }:

pkgs.stdenv.mkDerivation {
  pname = "remindd";
  version = "0.1.0";

  src = ../.;

  nativeBuildInputs = [ pkgs.microhs pkgs.stdenv.cc ];

  buildPhase = ''
    runHook preBuild
    cp ${dir}/main.hs main.hs
    mhs main.hs -o remindd
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 remindd $out/bin/remindd
    for f in ${dir}/*; do
      base=$(basename "$f")
      if [ "$base" != main.hs ] && [ -f "$f" ]; then
        install -Dm755 "$f" "$out/bin/$base"
      fi
    done
    runHook postInstall
  '';
}

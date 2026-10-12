# Compile a schedule directory into the `remindd` command.
#
# The directory must contain `main.hs`: a module that imports `Lib` and
# provides `main :: IO ()`.  Other `*.hs` files are compiled as modules `main`
# can import (but `Lib.hs` is reserved -- `Lib` comes from this flake).  Every
# /non-Haskell/ regular file is installed in `$out/libexec/remindd`, private to
# the program -- the schedule reaches it with `Lib.bundled`, so it never
# appears on `PATH`.
{ pkgs, dir }:

pkgs.stdenv.mkDerivation {
  pname = "remindd";
  version = "0.1.0";

  src = ../.;

  nativeBuildInputs = [ pkgs.microhs pkgs.stdenv.cc ];

  buildPhase = ''
    runHook preBuild
    cp ${dir}/main.hs main.hs
    for f in ${dir}/*.hs; do
      [ -f "$f" ] || continue
      base=$(basename "$f")
      if [ "$base" = main.hs ] || [ "$base" = Lib.hs ]; then
        continue
      fi
      cp "$f" "$base"
    done
    mhs main.hs -o remindd
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 remindd $out/bin/remindd
    for f in ${dir}/*; do
      [ -f "$f" ] || continue
      base=$(basename "$f")
      if [ "$base" = main.hs ] || [ "''${base##*.}" = hs ]; then
        continue
      fi
      if [ -x "$f" ]; then
        install -Dm755 "$f" "$out/libexec/remindd/$base"
      else
        install -Dm644 "$f" "$out/libexec/remindd/$base"
      fi
    done
    runHook postInstall
  '';
}

{ stdenv }:

stdenv.mkDerivation {
  name = "pipestatus-scripts";
  src = ./scripts;
  phases = "installPhase";
  installPhase = ''
    mkdir -p $out/bin
    cp -R $src/. $out/bin/
    chmod -R +x $out/bin/.
  '';
}

{ pkgs }:

rec {
  pipestatus = pkgs.haskellPackages.callCabal2nix "pipestatus" ./. { };
  pipestatus-dev = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          ghcid
          hlint
        ]);
  };
  pipestatus-wrapped = pkgs.callPackage ({ pipestatus, stdenv, makeWrapper }:
  stdenv.mkDerivation {
    phases = "installPhase";
    name = pipestatus.name + "-wrapped";
    nativeBuildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp ${pipestatus}/bin/pipestatus $out/bin/pipestatus-wrapped
      wrapProgram $out/bin/pipestatus-wrapped --prefix PATH : ${
        pkgs.dunst.override { dunstify = true; }
      }/bin/ ;

    '';
  }) { pipestatus = pipestatus; };
  scripts = pkgs.callPackage ./scripts.nix { };
}

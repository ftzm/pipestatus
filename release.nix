let pkgs = import <nixpkgs> { };
in rec {
  pipestatus = pkgs.haskellPackages.callCabal2nix "pipestatus" ./. { };
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
  shell = pkgs.haskellPackages.shellFor {
    packages = p: [ pipestatus ];
    buildInputs = with pkgs; [
      cabal-install
      hlint
      pkgs.haskellPackages.brittany
    ];
  };
  scripts = pkgs.callPackage ./scripts.nix { };
}

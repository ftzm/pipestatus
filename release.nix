let pkgs = import <nixpkgs> { };
in rec {
  pipestatus = pkgs.haskellPackages.callCabal2nix "pipestatus" ./. { };
  shell = pkgs.haskellPackages.shellFor {
    packages = p: [ pipestatus ];
    buildInputs = with pkgs; [
      cabal-install
      hlint
      pkgs.haskellPackages.brittany
    ];
  };
  ps_scripts = pkgs.callPackage ./scripts.nix { };
}

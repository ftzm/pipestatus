let pkgs = import <nixpkgs> { };
in {
  pipestatus = pkgs.haskellPackages.callPackage ./project.nix { };
  ps_scripts = pkgs.callPackage ./scripts.nix { };
}

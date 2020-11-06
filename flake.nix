{
  description = "pipestatus";

  outputs = { self, nixpkgs }:
    let
      packages = with import nixpkgs { system = "x86_64-linux"; };
          callPackage ./release.nix {};

    in
      {
        packages.x86_64-linux.pipestatus-bin = packages.pipestatus;
        packages.x86_64-linux.pipestatus-scripts = packages.scripts;
        defaultPackage.x86_64-linux = packages.x86_64-linux.pipestatus-bin;
        overlay = final: prev: {
          pipestatus = packages;
        };
      };
}

{ mkDerivation, base, containers, directory, hpack, lens
, megaparsec, process, split, stdenv, time, unix
}:
mkDerivation {
  pname = "pipestatus";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory lens megaparsec process split time unix
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers directory lens megaparsec process split time unix
  ];
  testHaskellDepends = [
    base containers directory lens megaparsec process split time unix
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/pipestatus#readme";
  license = stdenv.lib.licenses.bsd3;
}

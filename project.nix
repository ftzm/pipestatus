{ mkDerivation, base, containers, hpack, lens, process, split
, stdenv, unix
}:
mkDerivation {
  pname = "pipestatus";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers lens process split unix
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base containers lens process split unix
  ];
  testHaskellDepends = [ base containers lens process split unix ];
  preConfigure = "hpack";
  homepage = "https://github.com/githubuser/pipestatus#readme";
  license = stdenv.lib.licenses.bsd3;
}

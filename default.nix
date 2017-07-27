{ mkDerivation, base, binary, bytestring, mmap, optparse-generic
, parallel, stdenv, system-filepath, vector
}:
mkDerivation {
  pname = "sig";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring mmap optparse-generic parallel
    system-filepath vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/Gabriel439/Haskell-Sig-Library";
  description = "Blazing fast signature detection";
  license = stdenv.lib.licenses.bsd3;
}

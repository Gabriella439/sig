{ mkDerivation, base, binary, bytestring, mmap, parallel, stdenv
, vector
}:
mkDerivation {
  pname = "sig";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring mmap parallel vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/Gabriel439/Haskell-Sig-Library";
  description = "Blazing fast signature detection";
  license = stdenv.lib.licenses.bsd3;
}

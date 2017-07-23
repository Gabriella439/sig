{ mkDerivation, base, binary, bytestring, deepseq, dhall, mmap
, parallel, stdenv
}:
mkDerivation {
  pname = "sig";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring deepseq dhall mmap parallel
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/Gabriel439/Haskell-Sig-Library";
  description = "Blazing fast signature detection";
  license = stdenv.lib.licenses.bsd3;
}

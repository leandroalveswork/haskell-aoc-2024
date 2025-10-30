{ mkDerivation, base, lib, split }:
mkDerivation {
  pname = "haskell-aoc2024";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base split ];
  license = lib.licenses.bsd3;
  mainProgram = "haskell-aoc2024";
}

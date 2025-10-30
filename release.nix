let
  # 2025-08-07T16:20:54+05:30
  commit = "e728d7ae4bb6394bbd19eec52b7358526a44c414";
  src = fetchTarball "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";

  compiler = "ghc9101";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              haskell-aoc2024 =
                haskellPackagesNew.callPackage ./haskell-aoc2024.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import src { inherit config; overlays = [ ]; };
in
  pkgs.haskell.packages."${compiler}".haskell-aoc2024

let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          sig = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { inherit (pkgs.haskellPackages) sig;
  }

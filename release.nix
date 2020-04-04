let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: with pkgs.haskell.lib; rec {
        haskellPackages = pkgs.haskell.packages.ghc865.override {
            overrides = self: super: {
                persistent-postgresql = dontCheck super.persistent-postgresql;
                hauler-api = self.callPackage ./default.nix {};
            };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in pkgs.haskellPackages.hauler-api

let
  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        pkgsNew.lib.fold pkgsNew.lib.composeExtensions
          (old.overrides or (_: _: { }))
          [ (pkgsNew.haskell.lib.packageSourceOverrides {
              pipes = ./.;
            })
            (pkgsNew.haskell.lib.packagesFromDirectory {
              directory = ./nix;
            })
            (haskellPackagesNew: haskellPackagesOld: {
            })
          ];
    });
  };

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/1b55bc5d4b5cb6b35d71e7fe22cae9558c312937.tar.gz";

    sha256 = "05761c8bi6chlj15428h3k30r8b8g4w3h0m4xpsj6f9qcz27d9nf";
  };

  pkgs = import nixpkgs { config = { }; overlays = [ overlay ]; };

in
  { pipes = pkgs.haskellPackages.pipes;
  }

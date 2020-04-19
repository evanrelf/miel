let
  haskellPackagesOverlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        let
          overridePackages =
            pkgsNew.haskell.lib.packagesFromDirectory {
              directory = ./nix/haskell-packages;
            };
        in
          pkgsNew.lib.fold
            pkgsNew.lib.composeExtensions
            (old.overrides or (_: _: {}))
            [ overridePackages ];
    });
  };


  pkgs =
    import ./nix/nixpkgs.nix {
      overlays = [ haskellPackagesOverlay ];
    };
in
  pkgs.haskellPackages.miel

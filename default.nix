let
  pkgs = import ./nix/nixpkgs.nix { config = {}; };
in
  pkgs.haskellPackages.callCabal2nix "miel" ./. {}

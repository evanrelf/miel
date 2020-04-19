{ callCabal2nix, ... }:

let
  pkgs = import ../nixpkgs.nix {};

  src =
    pkgs.nix-gitignore.gitignoreSource [
      "/.git/"
      "/nix/"
      "/*.nix"
    ] ../../.;
in
  callCabal2nix "miel" src {}

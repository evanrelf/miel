args:

let
  # nixos-20.03 on 2020-04-19
  rev = "a17e021b9486a7699593f1a48f99f1a96a0dfc0e";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0d2vz17rcxy4h5fkwngzz3nmqnqgj3irkhz7hkbacgs03h69z8gh";
  };
in
  import nixpkgs ({ config = {}; } // args)

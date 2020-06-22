let
  withCompiler = compiler: (import ./. { inherit compiler; }).miel;

in
  { miel-ghc883 = withCompiler "ghc883";
    miel-ghc865 = withCompiler "ghc865";
  }

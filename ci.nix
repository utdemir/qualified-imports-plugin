let
compilers = [ "ghc8107Binary" "ghc902" "ghc924" ];
withCompiler = c: (import ./default.nix {compiler=c;}).qualified-imports-plugin;
in
builtins.listToAttrs
  (builtins.map
    (c: { name = c; value = withCompiler c; })
    compilers)

let
compilers = [ "ghc8104" "ghc901" ];
withCompiler = c: (import ./default.nix {compiler=c;}).qualified-imports-plugin;
in
builtins.listToAttrs
  (builtins.map
    (c: { name = c; value = withCompiler c; })
    compilers)

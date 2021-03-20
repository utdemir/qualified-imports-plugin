# qualified-imports-plugin

A GHC plugin to automatically insert common qualified imports.

Supports GHC 8.10 and GHC 9.

Example:

```
module Test where

f1 = Text.pack "test"
f2 = HashMap.empty
```

works, with a line on `cabal` file like:

```
ghc-options:   -fplugin=QualifiedImportsPlugin
build-depends: ...
             , qualified-imports-plugin
```

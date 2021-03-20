# qualified-imports-plugin

A GHC plugin to automatically insert common qualified imports. Supports
GHC 8.10 and GHC 9.

# Example

```haskell
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

It comes with (hopefully) sane [defaults][], but these can be extended, eg:
`-fplugin-opt=QualifiedImportsPlugin:Data.Graph:Graph`.  The defaults can
also be ommitted via `-fplugin-opt=QualifiedImportsPlugin:no-defaults`.

[defaults]: https://github.com/utdemir/qualified-imports-plugin/blob/main/src/QualifiedImportsPlugin.hs#L41-L60

# Background

The best practice when using Haskell modules these days is qualified
imports. This solves name clashes (without resorting to ad-hoc
polymorphism), and make the code easier to follow through.

However, this ends up introducing a bunch of import statements to most
real-world modules, it is not uncommon to see sections like

```haskell
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
```

Arguably this is unnecessary; everyone likely knows what `Text` is,
without looking at the import statements.

This plugin is a PoC to see what would it look like if we could omit
some of those.

My main use case in mind is using this alongside with the excellent
[relude][]; so, the default names reflect the naming convention there
(eg. `relude` has `LText` for `Data.Text.Lazy (Text)` hence this plugin
has `import qualified Data.Text.Lazy as LText` as a default).

[relude]: https://github.com/kowainik/relude

# Details

* It always adds the available imports, regardless if it is used or
not. This is to allow the suggestion in error messages to contain the
available modules:

```
    Not in scope: ‘Teext.pack’
    Perhaps you meant one of these:
      ‘Text.pack’ (imported from Data.Text),
      ‘LText.pack’ (imported from Data.Text.Lazy),
      ‘Text.unpack’ (imported from Data.Text)
    No module named ‘Teext’ is imported.
```

* It also adds the imports if an abbreviation used, but not possible
to import. This again to get easier to understand errors:

```
    Could not load module ‘Data.Text’
    It is a member of the hidden package ‘text-1.2.4.1’.
    Perhaps you need to add ‘text’ to the build-depends in your .cabal file.
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
4 | test1 = Text.pack "test1"
  |         ^^^^^^^^^
```

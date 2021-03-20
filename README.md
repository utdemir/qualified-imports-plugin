# qualified-imports-plugin

A GHC plugin to automatically insert common qualified imports. Supports
GHC 8.10 and GHC 9.

# Example

This plugin, alongside with [relude][], allows below code to compile,
without requiring explicit qualified imports.

```haskell
module Main where

countChars :: Text -> [(Char, Int)]
countChars txt =
  txt
    & Text.foldl
        (\m chr -> Map.alter (Just . maybe 1 (+1)) chr m)
        Map.empty
    & Map.toList
    & sortOn (Down . snd)

main :: IO ()
main =
  countChars (Text.pack "a peck of pickled peppers")
    & mapM_ print
```

With a `cabal` stanza like:

```
executable example
  main-is:          Main.hs
  ghc-options:      -fplugin=QualifiedImportsPlugin
  mixins:           base hiding (Prelude)
                  , relude (Relude as Prelude)
  build-depends:    base ^>=4.15.0.0
                  , relude
                  , text
                  , containers
                  , qualified-imports-plugin
  default-language: Haskell2010
```

See the complete [example][].

It comes with (hopefully) sane [defaults][], but these can be extended, eg:
`-fplugin-opt=QualifiedImportsPlugin:Data.Graph:Graph`.  The defaults can
also be ommitted via `-fplugin-opt=QualifiedImportsPlugin:no-defaults`.

[defaults]: https://github.com/utdemir/qualified-imports-plugin/blob/main/src/QualifiedImportsPlugin.hs#L41-L60
[example]: https://github.com/utdemir/qualified-imports-plugin/blob/main/example

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

* One still needs to add required packages to the `.cabal` file.

* The plugin imports the defined modules, regardless if they are used or
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

  In the presence of orphan/overlapping instances this can break/change
  the meaning of code.

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

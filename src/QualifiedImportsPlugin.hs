{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module QualifiedImportsPlugin where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.Main (getHscEnv)
import GHC.Hs
import GHC.Plugins hiding (getHscEnv, (<>))
import GHC.Utils.Error
import GHC.Data.Bag

unhelpfulOther :: FastString -> UnhelpfulSpanReason
unhelpfulOther = UnhelpfulOther
#else
import GHC.Hs hiding (HsModule)
import qualified GHC.Hs as GHC
import Bag
import GhcPlugins hiding (getHscEnv, (<>))
import ErrUtils
import HscMain

type HsModule = GHC.HsModule GhcPs

unitState :: a -> a
unitState = id

unhelpfulOther :: String -> FastString
unhelpfulOther = mkFastString
#endif

defaultImports :: [(String, String)]
defaultImports =
  [ ("Data.Text", "Text"),
    ("Data.Text.IO", "Text"),
    ("Data.Text.Lazy", "LText"),
    ("Data.Text.Lazy.IO", "LText"),
    ("Data.ByteString", "ByteString"),
    ("Data.ByteString.Lazy", "LByteString"),
    ("Data.Map.Strict", "Map"),
    ("Data.Map.Lazy", "LMap"),
    ("Data.IntMap.Strict", "IntMap"),
    ("Data.IntMap.Lazy", "LIntMap"),
    ("Data.HashMap.Strict", "HashMap"),
    ("Data.HashMap.Lazy", "LHashMap"),
    ("Data.HashSet", "HashSet"),
    ("Data.Set", "Set"),
    ("Data.Aeson", "Aeson"),
    ("Data.Vector", "Vector"),
    ("Data.Vector.Mutable", "MVector")
  ]

data Opts = Opts
  { optsNoDefaults :: Bool,
    optsCustomImports :: [(String, String)]
  }

instance Semigroup Opts where
  Opts a1 a2 <> Opts b1 b2 = Opts (a1 || b1) (a2 <> b2)

instance Monoid Opts where
  mempty = Opts False []

parseOpts :: [CommandLineOption] -> Hsc Opts
parseOpts [] = return mempty
parseOpts (x : xs) = case parseOpt x of
  Nothing -> do
    () <- Hsc $ \env wm ->
      -- There must a better way than delving into Hsc manually, but I couldn't find the function
      -- 'WarnMsg -> Hsc ()'.
      let msg =
            mkPlainWarnMsg
              (hsc_dflags env)
              (UnhelpfulSpan $ unhelpfulOther "QualifiedImportsPlugin")
              ("Unknown argument:" <+> text x)
       in return ((), consBag msg wm)
    parseOpts xs
  Just opts -> (opts <>) <$> parseOpts xs

{-
Valid options:
- no-defaults
- <actual_module_name>:<qualified_name>
-}
parseOpt :: CommandLineOption -> Maybe Opts
parseOpt "no-defaults" = Just $ mempty {optsNoDefaults = True}
parseOpt xs =
  -- Check if something looks like two module names separated by a colon. This still allows some
  -- invalid module names, but I think it is good enough.
  case span (/= ':') xs of
    (from, ':' : to)
      | all allowed from && all allowed to -> Just $ mempty {optsCustomImports = [(from, to)]}
    _ -> Nothing
  where
    allowed c =
      or
        [ 'a' <= c && c <= 'z',
          'A' <= c && c <= 'Z',
          c == '.'
        ]

plugin :: Plugin
plugin =
  defaultPlugin
    { pluginRecompile = purePlugin,
      parsedResultAction = \args _ parsed -> do
        opts <- parseOpts args
        nm <-
          hpm_module parsed
            <&> modifyHsMod opts
            & sequenceA
        return $ parsed {hpm_module = nm}
    }

{-
Insert qualified import statements to a module.

It's not very straightforward which imports to insert. We can not insert all of them,
because then GHC will complain about missing modules/modules in hidden packages. We can
only insert the ones accessed from the code, but then if the user makes a typo in the name,
GHC won't suggest the correct names.

So:

1. We start with a combination of 'defaultImports' and user specified imports.
2. For every candidate import, we insert if it's either:
   * Referred from the source code
   * Available to import
3. When we insert an import which if referred from the source code, we use the source
   location of the referent as the source code of the import, in order for the error
   message to be useful.
-}
modifyHsMod :: Opts -> HsModule -> Hsc HsModule
modifyHsMod opts m = do
  env <- getHscEnv

  let imports =
        (if optsNoDefaults opts then [] else defaultImports)
          ++ optsCustomImports opts

      refs = referencedModules m
      newImports =
        imports
          & map (\(n, qn) -> (mkModuleName n, mkModuleName qn))
          & mapMaybe
            ( \(n, qn) ->
                case Map.lookup qn refs of
                  Nothing
                    | isModuleAvailable env n -> Just (Nothing, n, qn)
                    | otherwise -> Nothing
                  Just loc ->
                    Just (Just loc, n, qn)
            )
          & map
            ( \(loc, n, qn) ->
                (maybe noLoc L loc)
                  (simpleImportDecl n)
                    { ideclQualified = QualifiedPre,
                      ideclAs = Just (noLoc qn),
                      -- This makes GHC not complain about unused imports.
                      ideclImplicit = True
                    }
            )
  return $ m {hsmodImports = hsmodImports m ++ newImports}

-- Figure out if the module is available to import.
isModuleAvailable :: HscEnv -> ModuleName -> Bool
isModuleAvailable env n =
  let us = unitState $ hsc_dflags env
   in case lookupModuleWithSuggestions us n Nothing of
        LookupFound _ _ -> True -- "found"
        LookupMultiple _ -> False -- "multiple"
        LookupHidden _ _ -> False -- "hidden " ++ show (length l) ++ " " ++ show (length r)
        LookupUnusable _ -> False -- "unusable"
        LookupNotFound _ -> False -- "not found"

-- We also carry the references to modules, so we can the error messages can point to
-- the use site (since there is no visible import statement).
referencedModules :: HsModule -> Map ModuleName SrcSpan
referencedModules m =
  hsmodDecls m
    & map go
    & reverse
    & mconcat
  where
    go :: Data a => a -> Map ModuleName SrcSpan
    go =
      gmapQr
        (flip mappend)
        mempty
        ( \d ->
            case cast @_ @(Located RdrName) d of
              Nothing -> go d
              Just (L loc (Qual m _)) -> Map.singleton m loc
              Just _ -> mempty
        )

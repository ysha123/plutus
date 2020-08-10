{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for compiling GHC kinds into PlutusCore kinds.
module Language.PlutusTx.Compiler.Kind (compileKind) where

import qualified GhcPlugins as GHC
import qualified Kind as GHC
import qualified Language.PlutusCore as PLC
import Language.PlutusTx.Compiler.Error
import Language.PlutusTx.Compiler.Types
import Language.PlutusTx.Compiler.Utils

compileKind :: Compiling uni m => GHC.Kind -> m (PLC.Kind ())
compileKind k = withContextM 2 (sdToTxt $ "Compiling kind:" GHC.<+> GHC.ppr k) $ case k of
  -- this is a bit weird because GHC uses 'Type' to represent kinds, so '* -> *' is a 'TyFun'
  (GHC.isLiftedTypeKind -> True) -> pure $ PLC.Type ()
  (GHC.splitFunTy_maybe -> Just (i, o)) -> PLC.KindArrow () <$> compileKind i <*> compileKind o
  _ -> throwSd UnsupportedError $ "Kind:" GHC.<+> GHC.ppr k

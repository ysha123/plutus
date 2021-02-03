{-# LANGUAGE LambdaCase #-}
-- | Create a lookup(GHC.Name) function to return the core unfoldings stored in "extensible" interface files (.hi)
module Language.PlutusTx.LookupIface
    ( mkLookupIf
    ) where

import           BinIface
import           CoreSyn
import           Data.Bifunctor
import           Data.IORef
import           Data.Maybe
import           HscTypes
import           Id
import           IfaceEnv
import           IfaceSyn
import           Maybes
import           Module
import           Name
import           NameEnv
import           Outputable
import           TcIface
import           TcRnMonad

{- |
Initialise a stateful `IO` function for loading core bindings by loading the
relevant `ModIface` from disk. Each module interface that is loaded has its bindings
cached within using `IORef`.

The current module doesn't have an interface file yet, so we recover its binds
from the `ModGuts` instead. However, since we're running this plugin early in
the core pipeline, the current module's binds won't have passed through the
core optimisation phases yet, so if we're inlining `Name`s from this module
we'll have to rely on the optimisations being run on our already inlined ASTs.
-}
mkLookupIf :: HscEnv
           -> ModGuts
           -> IO (Name -> IO (Maybe CoreBind))
mkLookupIf hscEnv guts = do
  let binds = bindsToNameEnv $ mg_binds guts
  newCache <- newIORef $ extendModuleEnv emptyModuleEnv (mg_module guts) (Just binds)
  pure $ lookupIf newCache hscEnv

{-
Return the `Bind` for the given `Name`. Cache the results of disk lookups.
-}
lookupIf :: IORef (ModuleEnv (Maybe (NameEnv CoreBind))) -- ^ the cache of already looked-up modules
         -> HscEnv
         -> Name -- ^ the ghc name we are looking after
         -> IO (Maybe CoreBind)
lookupIf cache env name = do
    eps      <- hscEPS env
    modBinds <- readIORef cache
    case nameModule_maybe name of
        Just modu ->
            case lookupModuleEnv modBinds modu of
                -- Cache hit:
                -- Either it is in the local module (has no iface yet, thus looking at modguts)
                -- or in an external module which we have loaded its iface before
                Just (Just binds) -> pure $ lookupNameEnv binds name
                -- Cache hit:
                -- It is in an external module that we tried to load its iface before but failed,
                -- probably because it was not compiled with the patched-ghc.
                Just Nothing -> pure Nothing
                -- Cache miss:
                -- Try and import the iface of the *external* module. A local module always produces a cache hit.
                Nothing -> do
                    let extendCache = writeIORef cache . extendModuleEnv modBinds modu
                    case lookupIfaceByModule (hsc_HPT env) (eps_PIT eps) modu of
                        Just iface -> do
                            bnds <- initIfaceLoad env $
                                     -- False signifies that is is not an .hs-boot file
                                     -- FIXME: instead of False put NotBoot for >=GHC9
                                     initIfaceLcl (mi_semantic_module iface) (text "core") False $
                                        loadCoreBindings iface
                            case bnds of
                                Just bds -> do
                                    let binds' = bindsToNameEnv bds
                                    extendCache $ Just binds'
                                    pure $ lookupNameEnv binds' name
                                Nothing -> do
                                    -- failed to load the binds cache the failure
                                    extendCache Nothing
                                    pure Nothing
                        Nothing -> do
                            -- failed to load the iface, cache the failure
                            extendCache Nothing
                            pure Nothing
        Nothing -> pure Nothing

{- From a list of bindings, build a mapping of name to a binding.
Note that a mutually recursive binding will contain more than one names;
we duplicate the same mutually recursive binding to each name key in the map.
-}
bindsToNameEnv :: [CoreBind] -> NameEnv CoreBind
bindsToNameEnv = mkNameEnv . concatMap bindToNameEntry
  where
    bindToNameEntry :: CoreBind -> [(Name, CoreBind)]
    bindToNameEntry b = case b of
        NonRec n _ -> [(idName n,b)]
        Rec assocs -> bimap idName (const b) <$> assocs

{-
Perform interface `typecheck` loading from this binding's extensible interface
field within the deserialised `ModIface` to load the bindings that the field
contains, if the field exists.
-}
loadCoreBindings :: ModIface -> IfL (Maybe [CoreBind])
loadCoreBindings iface = do
  ncu <- mkNameCacheUpdater
  mbinds <- liftIO $ readIfaceFieldWith "ghc/phase/core" (getWithUserData ncu) iface
  case mbinds of
    Just ibinds -> Just . catMaybes <$> traverse tcIfaceBinding ibinds
    Nothing     -> pure Nothing

-------------------------------------------------------------------------------
-- Interface loading for top-level bindings
-------------------------------------------------------------------------------

{-
Certain RHSs fail to typecheck due to the error `Iface id out of scope: ...`.
In particular, this workaround is used to exclude GHC's special type reflection
bindings from causing problems in loading.

In theory, we should be removing them during serialisation, but they are structured
as real bindings, so we would have to do a fragile test on the `Name`.
-}
tcIfaceBinding :: IfaceBinding -> IfL (Maybe CoreBind)
tcIfaceBinding ibind =
  rightToMaybe <$> tryAllM (tcIfaceBinding' ibind)
  where
    tcIfaceBinding' :: IfaceBinding -> IfL CoreBind
    tcIfaceBinding' = \case
        IfaceNonRec letbndr rhs -> uncurry NonRec <$> go letbndr rhs
        IfaceRec    pairs       -> Rec <$> traverse (uncurry go) pairs
    go :: IfaceLetBndr
       -> IfaceExpr
       -> IOEnv (Env IfGblEnv IfLclEnv) (Id, CoreExpr)
    go (IfLetBndr fs ty _info ji) rhs = do
        name <- lookupIfaceTop $ mkVarOccFS fs
        ty'  <- tcIfaceType ty
        rhs' <- tcIfaceExpr rhs
        let ident = mkExportedVanillaId name ty' `asJoinId_maybe` tcJoinInfo ji
        pure (ident, rhs')


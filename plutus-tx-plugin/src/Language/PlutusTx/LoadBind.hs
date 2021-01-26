module Language.PlutusTx.LoadBind
    ( newLoadBind
    ) where

import           BinIface
import           CoreSyn
import           Data.IORef
import           Data.Maybe
import Data.Bifunctor
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

{-
Initialise a stateful `IO` function for loading core bindings by loading the
relevant `ModIface` from disk. Each interface that is loaded has its bindings
cached within an `IORef (ModuleEnv (Maybe (NameEnv (Bind CoreBndr))))`.

The current module doesn't have an interface file yet, so we recover its binds
from the `ModGuts` instead. However, since we're running this plugin early in
the core pipeline, the current module's binds won't have passed through the
core optimisation phases yet, so if we're inlining `Name`s from this module
we'll have to rely on the optimisations being run on our already inlined ASTs.
-}
newLoadBind :: HscEnv -> ModGuts -> IO (Name -> IO (Maybe (Bind CoreBndr)))
newLoadBind hscEnv guts = do
  let binds = bindsToNameEnv $ mg_binds guts
  modBindsR <- newIORef (extendModuleEnv emptyModuleEnv (mg_module guts) (Just binds))
  return (loadBind modBindsR hscEnv)

{-
Return the `Bind` for the given `Name`. Cache the results of disk lookups.
-}
loadBind :: IORef (ModuleEnv (Maybe (NameEnv (Bind CoreBndr))))
         -> HscEnv
         -> Name
         -> IO (Maybe (Bind CoreBndr))
loadBind modBindsR env name = do
  eps      <- hscEPS env
  modBinds <- readIORef modBindsR
  case nameModule_maybe name of
   Just modu -> do
      case lookupModuleEnv modBinds modu of
        -- Cache hit:
        -- Either it is in the local module (has no iface yet, thus looking at modguts)
        -- or in an external module which we have loaded its iface before
        Just (Just binds) -> return $ lookupNameEnv binds name
        -- Cache hit:
        -- It is in an external module that we tried to load its iface before but failed,
        -- probably because it was not compiled with the patched-ghc.
        Just Nothing -> return Nothing
        -- Cache miss:
        -- Try and import the iface of the *external* module. A local module always produces a cache hit.
        Nothing ->
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
                            writeIORef modBindsR (extendModuleEnv modBinds modu (Just binds'))
                            return $ lookupNameEnv binds' name
                        Nothing -> do
                            -- failed to load the iface
                            writeIORef modBindsR (extendModuleEnv modBinds modu Nothing)
                            return Nothing
                _ -> do
                    -- failed to load the iface
                    writeIORef modBindsR (extendModuleEnv modBinds modu Nothing)
                    return Nothing
   _ -> return Nothing

{- From a list of binds construct a mapping of name to each bind.
Note that in case of a mutually recursive function (names), the bind will contain more than one names;
for each such name a key will be created in the mapping and its value will be the same bind duplicated
and distributed among all these mutually recursive names.
-}
bindsToNameEnv :: [Bind Id] -> NameEnv (Bind Id)
bindsToNameEnv = mkNameEnv . concatMap bindToNameEntry
  where
    bindToNameEntry :: Bind Id -> [(Name, Bind Id)]
    bindToNameEntry b@(NonRec n _) = [(idName n,b)]
    bindToNameEntry b@(Rec assocs) = bimap idName (const b) <$> assocs


{-
Perform interface `typecheck` loading from this binding's extensible interface
field within the deserialised `ModIface` to load the bindings that the field
contains, if the field exists.
-}
loadCoreBindings :: ModIface -> IfL (Maybe [Bind CoreBndr])
loadCoreBindings iface = do
  ncu <- mkNameCacheUpdater
  mbinds <- liftIO (readIfaceFieldWith "ghc/phase/core" (getWithUserData ncu) iface)
  case mbinds of
    Just ibinds -> Just . catMaybes <$> mapM tcIfaceBinding ibinds
    Nothing     -> return Nothing

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
tcIfaceBinding :: IfaceBinding -> IfL (Maybe (Bind Id))
tcIfaceBinding ibind =
  rightToMaybe <$> tryAllM (tcIfaceBinding' ibind)

tcIfaceBinding' :: IfaceBinding -> IfL (Bind Id)
tcIfaceBinding' b =
  case b of
    IfaceNonRec letbndr rhs -> uncurry NonRec <$> go letbndr rhs
    IfaceRec    pairs       -> Rec <$> mapM (uncurry go) pairs
  where
    go (IfLetBndr fs ty _info ji) rhs = do
        name <- lookupIfaceTop (mkVarOccFS fs)
        ty'  <- tcIfaceType ty
        rhs' <- tcIfaceExpr rhs
        let ident = mkExportedVanillaId name ty' `asJoinId_maybe` tcJoinInfo ji
        return (ident, rhs')


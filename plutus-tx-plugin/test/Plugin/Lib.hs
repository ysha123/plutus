{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plugin.Lib where

import Codec.Serialise (Serialise)
import Common
import Data.Text.Prettyprint.Doc
import Language.Haskell.TH
import Language.PlutusCore.Pretty (PrettyConst)
import qualified Language.PlutusCore.Universe as PLC
import qualified Language.PlutusTx.Builtins as Builtins
import Language.PlutusTx.Code
import Language.PlutusTx.Evaluation
import Language.PlutusTx.Prelude
import Language.PlutusTx.TH
import PlcTestUtils

{-# ANN module "HLint: ignore" #-}

instance
  (PLC.Closed uni, uni `PLC.Everywhere` Serialise) =>
  GetProgram (CompiledCode uni a) uni
  where
  getProgram = catchAll . getPlc

goldenPir ::
  (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PrettyConst, uni `PLC.Everywhere` Serialise) =>
  String ->
  CompiledCode uni a ->
  TestNested
goldenPir name value = nestedGoldenVsDoc name $ pretty $ getPir value

-- This is here for the Plugin spec, but we're testing using things from a different module
andExternal :: Bool -> Bool -> Bool
andExternal a b = if a then b else False

data MyExternalRecord = MyExternalRecord {myExternal :: Integer}

{-# INLINEABLE evenDirect #-}
evenDirect :: Integer -> Bool
evenDirect n = if Builtins.equalsInteger n 0 then True else oddDirect (Builtins.subtractInteger n 1)

{-# INLINEABLE oddDirect #-}
oddDirect :: Integer -> Bool
oddDirect n = if Builtins.equalsInteger n 0 then False else evenDirect (Builtins.subtractInteger n 1)

-- GHC will lift out the error call to the top level, which is unsafe unless we bind it lazily.
-- This is in Lib so we get the fully optimized unfolding with awkward top-level binds and everything.
joinError :: Bool -> Bool -> ()
joinError x y = if andExternal x y then Builtins.error () else ()

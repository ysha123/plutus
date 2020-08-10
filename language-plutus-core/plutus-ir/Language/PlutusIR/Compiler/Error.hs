{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.PlutusIR.Compiler.Error (Error (..), AsError (..)) where

import Control.Exception
import Control.Lens
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Typeable
import qualified Language.PlutusCore as PLC
import qualified Language.PlutusCore.Pretty as PLC

data Error uni a
  = -- | A generic compilation error.
    CompilationError a T.Text
  | -- | An error relating specifically to an unsupported feature.
    UnsupportedError a T.Text
  | -- | An error from running some PLC function, lifted into this error type for convenience.
    PLCError (PLC.Error uni a)
  deriving (Typeable)

makeClassyPrisms ''Error

instance PLC.AsTypeError (Error uni a) uni a where
  _TypeError = _PLCError . PLC._TypeError

instance
  (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
  Show (Error uni a)
  where
  show e = show $ PLC.prettyPlcClassicDebug e

instance
  (PLC.GShow uni, PLC.Closed uni, uni `PLC.Everywhere` PLC.PrettyConst, PP.Pretty a) =>
  PLC.PrettyBy PLC.PrettyConfigPlc (Error uni a)
  where
  prettyBy config = \case
    CompilationError x e -> "Error during compilation:" <+> PP.pretty e <> "(" <> PP.pretty x <> ")"
    UnsupportedError x e -> "Unsupported construct:" <+> PP.pretty e <+> "(" <> PP.pretty x <> ")"
    PLCError e -> PP.vsep ["Error from the PLC compiler:", PLC.prettyBy config e]

instance
  ( PLC.GShow uni,
    PLC.Closed uni,
    uni `PLC.Everywhere` PLC.PrettyConst,
    PP.Pretty a,
    Typeable uni,
    Typeable a
  ) =>
  Exception (Error uni a)

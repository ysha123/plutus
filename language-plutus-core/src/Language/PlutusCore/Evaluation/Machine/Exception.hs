{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- appears in the generated instances
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- | The exceptions that an abstract machine can throw.
module Language.PlutusCore.Evaluation.Machine.Exception
  ( UnliftingError (..),
    AsUnliftingError (..),
    ConstAppError (..),
    AsConstAppError (..),
    MachineError (..),
    AsMachineError (..),
    EvaluationError (..),
    AsEvaluationError (..),
    ErrorWithCause (..),
    MachineException,
    EvaluationException,
    throwingWithCause,
    extractEvaluationResult,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Language.PlutusCore.Evaluation.Result
import Language.PlutusCore.Pretty
import PlutusPrelude

-- | When unlifting of a PLC term into a Haskell value fails, this error is thrown.
newtype UnliftingError
  = UnliftingErrorE Text
  deriving (Show, Eq)
  deriving newtype (IsString, Semigroup)

-- | The type of constant applications errors (i.e. errors that may occur during evaluation of
-- a builtin function applied to some arguments).
data ConstAppError term
  = -- | A constant is applied to more arguments than needed in order to reduce.
    -- Note that this error occurs even if an expression is well-typed, because
    -- constant application is supposed to be computed as soon as there are enough arguments.
    ExcessArgumentsConstAppError [term]
  | -- | Could not construct denotation for a builtin.
    UnliftingConstAppError UnliftingError
  deriving (Show, Eq, Functor)

-- | Errors which can occur during a run of an abstract machine.
data MachineError err term
  = -- | An attempt to reduce a not immediately reducible type instantiation.
    NonPrimitiveInstantiationMachineError
  | -- | An attempt to unwrap a not wrapped term.
    NonWrapUnwrappedMachineError
  | -- | An attempt to reduce a not immediately reducible application.
    NonPrimitiveApplicationMachineError
  | -- | An attempt to evaluate an open term.
    OpenTermEvaluatedMachineError
  | -- | An attempt to compute a constant application resulted in 'ConstAppError'.
    ConstAppMachineError (ConstAppError term)
  | OtherMachineError err
  deriving (Show, Eq, Functor)

-- | The type of errors (all of them) which can occur during evaluation
-- (some are used-caused, some are internal).
data EvaluationError internal user term
  = -- | Indicates bugs.
    InternalEvaluationError (MachineError internal term)
  | -- | Indicates user errors.
    UserEvaluationError user
  deriving (Show, Eq)

mtraverse
  makeClassyPrisms
  [ ''UnliftingError,
    ''ConstAppError,
    ''MachineError,
    ''EvaluationError
  ]

instance AsMachineError (EvaluationError internal user term) internal term where
  _MachineError = _InternalEvaluationError

instance AsConstAppError (MachineError err term) term where
  _ConstAppError = _ConstAppMachineError

instance AsConstAppError (EvaluationError internal user term) term where
  _ConstAppError = _InternalEvaluationError . _ConstAppMachineError

instance AsUnliftingError (ConstAppError term) where
  _UnliftingError = _UnliftingConstAppError

instance AsUnliftingError (EvaluationError internal user term) where
  _UnliftingError = _InternalEvaluationError . _UnliftingConstAppError

instance AsUnliftingError (MachineError err term) where
  _UnliftingError = _ConstAppMachineError . _UnliftingConstAppError

-- | An error and (optionally) what caused it.
data ErrorWithCause err term
  = ErrorWithCause err (Maybe term)
  deriving (Eq, Functor)

type MachineException internal term = ErrorWithCause (MachineError internal term) term

type EvaluationException internal user term = ErrorWithCause (EvaluationError internal user term) term

-- | "Prismatically" throw an error and its (optional) cause.
throwingWithCause ::
  MonadError (ErrorWithCause e term) m =>
  AReview e t ->
  t ->
  Maybe term ->
  m x
throwingWithCause l t cause = reviews l (\e -> throwError $ ErrorWithCause e cause) t

-- | Turn any 'UserEvaluationError' into an 'EvaluationFailure'.
extractEvaluationResult ::
  Either (EvaluationException internal user term) a ->
  Either (MachineException internal term) (EvaluationResult a)
extractEvaluationResult (Right term) = Right $ EvaluationSuccess term
extractEvaluationResult (Left (ErrorWithCause evalErr cause)) = case evalErr of
  InternalEvaluationError err -> Left $ ErrorWithCause err cause
  UserEvaluationError _ -> Right $ EvaluationFailure

instance Pretty UnliftingError where
  pretty (UnliftingErrorE err) =
    fold
      [ "Could not unlift a builtin:",
        hardline,
        pretty err
      ]

instance
  (PrettyBy config term, HasPrettyDefaults config ~ 'True) =>
  PrettyBy config (ConstAppError term)
  where
  prettyBy config (ExcessArgumentsConstAppError args) =
    fold
      [ "A constant applied to too many arguments:",
        "\n",
        "Excess ones are: ",
        prettyBy config args
      ]
  prettyBy _ (UnliftingConstAppError err) = pretty err

instance
  (PrettyBy config term, HasPrettyDefaults config ~ 'True, Pretty err) =>
  PrettyBy config (MachineError err term)
  where
  prettyBy _ NonPrimitiveInstantiationMachineError =
    "Cannot reduce a not immediately reducible type instantiation."
  prettyBy _ NonWrapUnwrappedMachineError =
    "Cannot unwrap a not wrapped term."
  prettyBy _ NonPrimitiveApplicationMachineError =
    "Cannot reduce a not immediately reducible application."
  prettyBy _ OpenTermEvaluatedMachineError =
    "Cannot evaluate an open term."
  prettyBy config (ConstAppMachineError constAppError) =
    prettyBy config constAppError
  prettyBy _ (OtherMachineError err) =
    pretty err

instance
  ( PrettyBy config term,
    HasPrettyDefaults config ~ 'True,
    Pretty internal,
    Pretty user
  ) =>
  PrettyBy config (EvaluationError internal user term)
  where
  prettyBy config (InternalEvaluationError err) =
    fold
      [ "Internal error:",
        hardline,
        prettyBy config err
      ]
  prettyBy _ (UserEvaluationError err) =
    fold
      [ "User error:",
        hardline,
        pretty err
      ]

instance
  (PrettyBy config term, PrettyBy config err) =>
  PrettyBy config (ErrorWithCause err term)
  where
  prettyBy config (ErrorWithCause err mayCause) =
    "An error has occurred: " <+> prettyBy config err
      <> case mayCause of
        Nothing -> mempty
        Just cause -> hardline <> "Caused by:" <+> prettyBy config cause

instance
  (PrettyPlc term, PrettyPlc err) =>
  Show (ErrorWithCause err term)
  where
  show = render . prettyPlcReadableDebug

instance
  (PrettyPlc term, PrettyPlc err, Typeable term, Typeable err) =>
  Exception (ErrorWithCause err term)

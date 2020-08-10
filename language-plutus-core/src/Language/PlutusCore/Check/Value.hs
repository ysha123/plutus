{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Check.Value
  ( isTermValue,
  )
where

import Data.Either
import Language.PlutusCore.Core
import Language.PlutusCore.Error

isTermValue :: Term tyname name uni ann -> Bool
isTermValue = isRight . termValue

termValue :: Term tyname name uni ann -> Either (NormCheckError tyname name uni ann) ()
termValue (IWrap _ _ _ term) = termValue term
termValue LamAbs {} = pure ()
termValue TyAbs {} = pure ()
termValue Constant {} = pure ()
termValue t = Left $ BadTerm (termAnn t) t "term value"

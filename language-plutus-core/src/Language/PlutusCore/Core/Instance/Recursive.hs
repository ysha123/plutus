{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PlutusCore.Core.Instance.Recursive
  ( -- * Base functors
    TermF (..),
    TypeF (..),
    KindF (..),
  )
where

import Data.Functor.Foldable.TH
import Language.PlutusCore.Core.Type
import PlutusPrelude

$(join <$> traverse makeBaseFunctor [''Kind, ''Type, ''Term])

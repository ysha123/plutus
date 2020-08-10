{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Language.PlutusTx.IsData.Class where

import Data.ByteString.Lazy as BSL
import Data.Kind
import Data.Void
import Language.PlutusTx.Applicative
import Language.PlutusTx.Data
import Language.PlutusTx.Functor
import Prelude (Integer, Maybe (..))

{-# ANN module "HLint: ignore" #-}

-- | A typeclass for types that can be converted to and from 'Data'.
class IsData (a :: Type) where
  toData :: a -> Data

  -- TODO: this should probably provide some kind of diagnostics
  fromData :: Data -> Maybe a

instance IsData Data where
  {-# INLINEABLE toData #-}
  toData d = d
  {-# INLINEABLE fromData #-}
  fromData d = Just d

instance IsData Integer where
  {-# INLINEABLE toData #-}
  toData = I
  {-# INLINEABLE fromData #-}
  fromData (I i) = Just i
  fromData _ = Nothing

instance IsData ByteString where
  {-# INLINEABLE toData #-}
  toData b = B b
  {-# INLINEABLE fromData #-}
  fromData (B b) = Just b
  fromData _ = Nothing

instance IsData a => IsData [a] where
  {-# INLINEABLE toData #-}
  toData xs = List (fmap toData xs)
  {-# INLINEABLE fromData #-}
  fromData (List ds) = traverse fromData ds
  fromData _ = Nothing

instance IsData Void where
  {-# INLINEABLE toData #-}
  toData v = absurd v
  {-# INLINEABLE fromData #-}
  fromData _ = Nothing

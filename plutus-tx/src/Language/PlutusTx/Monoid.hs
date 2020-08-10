{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.PlutusTx.Monoid (Monoid (..), mappend, mconcat, Group (..), gsub) where

import qualified Language.PlutusTx.Builtins as Builtins
import Language.PlutusTx.Semigroup
import Prelude hiding (Monoid (..), Semigroup (..), mconcat)

{-# ANN module ("HLint: ignore" :: String) #-}

class Semigroup a => Monoid a where
  mempty :: a

-- mappend and mconcat deliberately omitted, to make this a one-method class which has a
-- simpler representation

{-# INLINEABLE mappend #-}
mappend :: Monoid a => a -> a -> a
mappend = (<>)

{-# INLINEABLE mconcat #-}

-- | Fold a list using the monoid.
mconcat :: Monoid a => [a] -> a
mconcat = foldr mappend mempty

instance Monoid Builtins.ByteString where
  {-# INLINEABLE mempty #-}
  mempty = Builtins.emptyByteString

instance Monoid Builtins.String where
  {-# INLINEABLE mempty #-}
  mempty = Builtins.emptyString

instance Monoid [a] where
  {-# INLINEABLE mempty #-}
  mempty = []

instance Semigroup a => Monoid (Maybe a) where
  {-# INLINEABLE mempty #-}
  mempty = Nothing

instance Monoid () where
  {-# INLINEABLE mempty #-}
  mempty = ()

class Monoid a => Group a where
  inv :: a -> a

{-# INLINEABLE gsub #-}
gsub :: Group a => a -> a -> a
gsub x y = x <> inv y

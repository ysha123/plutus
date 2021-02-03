{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Language.PlutusTx.Functor (Functor(..), (<$>), (<$), const, id) where

import           Data.Functor.Identity (Identity (..))
import           Prelude               hiding (Functor (..), const, id, (<$), (<$>))

{-# ANN module ("HLint: ignore"::String) #-}

{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g
-}
class Functor f where
    fmap :: (a -> b) -> f a -> f b

    -- (<$) deliberately omitted, to make this a one-method class which has a
    -- simpler representation

infixl 4 <$>
-- | An infix synonym for 'fmap'.
{-# NOINLINE (<$>) #-}
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) f fa = fmap f fa

infixl 4 <$
{-# NOINLINE (<$) #-}
-- | Replace all locations in the input with the same value.
(<$) :: Functor f => a -> f b -> f a
(<$) a fb = fmap (const a) fb

instance Functor [] where
    {-# NOINLINE fmap #-}
    fmap f l = case l of
            []   -> []
            x:xs -> f x : fmap f xs

instance Functor Maybe where
    {-# NOINLINE fmap #-}
    fmap f (Just a) = Just (f a)
    fmap _ Nothing  = Nothing

instance Functor (Either c) where
    {-# NOINLINE fmap #-}
    fmap f (Right a) = Right (f a)
    fmap _ (Left c)  = Left c

instance Functor ((,) c) where
    {-# NOINLINE fmap #-}
    fmap f (c, a) = (c, f a)

instance Functor Identity where
    {-# NOINLINE fmap #-}
    fmap f (Identity a) = Identity (f a)

{-# NOINLINE const #-}
-- | Plutus Tx version of 'Prelude.const'.
const :: a -> b -> a
const x _ =  x

{-# NOINLINE id #-}
-- | Plutus Tx version of 'Prelude.id'.
id :: a -> a
id x = x

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Language.PlutusTx.Applicative where

import           Language.PlutusTx.Functor
import           Prelude                   (Bool, Either (..), Maybe (..))

{-# ANN module "HLint: ignore" #-}

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
    {-# MINIMAL pure, (<*>) #-}
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

{-# NOINLINE liftA2 #-}
-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)

{-# NOINLINE (*>) #-}
-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
a1 *> a2 = (id <$ a1) <*> a2

{-# NOINLINE (<*) #-}
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

{-# NOINLINE traverse #-}
-- | Run an applicative function over a list of inputs.
traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse _ []    = pure []
traverse f (h:t) = (:) <$> f h <*> traverse f t

{-# NOINLINE sequence #-}
-- | Sequence a list of applicative actions.
sequence :: Applicative f => [f a] -> f [a]
sequence = traverse id

{-# NOINLINE unless #-}
unless :: (Applicative f) => Bool -> f () -> f ()
unless p s =  if p then pure () else s

instance Applicative Maybe where
    {-# NOINLINE pure #-}
    pure = Just
    {-# NOINLINE (<*>) #-}
    Nothing <*> _     = Nothing
    _ <*> Nothing     = Nothing
    Just f <*> Just x = Just (f x)

instance Applicative (Either a) where
    {-# NOINLINE pure #-}
    pure = Right
    {-# NOINLINE (<*>) #-}
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r

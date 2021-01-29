{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}

{- This module defines Quantifications, which are used together with
   forAllQ in DynamicLogic. A Quantification t can be used to generate
   an t, shrink a t, and recognise a generated t.
-}

module TypedStateModel.Quantify(
                Quantification(isaQ),
                generateQ, shrinkQ,
                arbitraryQ,exactlyQ,elementsQ,oneofQ,frequencyQ,mapQ,whereQ,
                Quantifiable(..)
               ) where

import           Data.Typeable

import           Test.QuickCheck

data Quantification a = Quantification
  { genQ :: Gen a,
    isaQ :: a -> Bool,
    shrQ :: a -> [a]
  }

generateQ :: Quantification a -> Gen a
generateQ q = genQ q `suchThat` isaQ q

shrinkQ :: Quantification a -> a -> [a]
shrinkQ q a = filter (isaQ q) (shrQ q a)

arbitraryQ :: Arbitrary a => Quantification a
arbitraryQ = Quantification arbitrary (const True) shrink

exactlyQ :: Eq a => a -> Quantification a
exactlyQ a = Quantification
  (return a)
  (==a)
  (const [])

elementsQ :: Eq a => [a] -> Quantification a
elementsQ as = Quantification (elements as) (`elem` as) (\a -> takeWhile (/=a) as)

frequencyQ :: [(Int,Quantification a)] -> Quantification a
frequencyQ iqs =
  Quantification
    (frequency [(i,genQ q) | (i,q) <- iqs])
    (isa iqs)
    (shr iqs)
  where isa [] a          = False
        isa ((i,q):iqs) a = (i > 0 && isaQ q a) || isa iqs a
        shr [] a = []
        shr ((i,q):iqs) a = [a' | i > 0, isaQ q a, a' <- shrQ q a]
                         ++ shr iqs a

oneofQ :: [Quantification a] -> Quantification a
oneofQ qs = frequencyQ $ map (1,) qs

mapQ :: (a -> b, b -> a) -> Quantification a -> Quantification b
mapQ (f,g) q = Quantification
  (f <$> genQ q)
  (isaQ q . g)
  (map f . shrQ q . g)

whereQ :: Quantification a -> (a -> Bool) -> Quantification a
whereQ q p = Quantification
  (genQ q `suchThat` p)
  (\a -> p a && isaQ q a)
  (\a -> if p a then filter p (shrQ q a) else [])

pairQ q q' = Quantification
  ((,) <$> genQ q <*> genQ q')
  (\(a,a') -> isaQ q a && isaQ q' a')
  (\(a,a') -> map (,a') (shrQ q a) ++ map (a,) (shrQ q' a'))

class (Eq (Quantifies q), Show (Quantifies q), Typeable (Quantifies q))
        => Quantifiable q where
  type Quantifies q
  quantify :: q -> Quantification (Quantifies q)

instance (Eq a, Show a, Typeable a) => Quantifiable (Quantification a) where
  type Quantifies (Quantification a) = a
  quantify = id

instance (Quantifiable a, Quantifiable b) => Quantifiable (a,b) where
  type Quantifies (a,b) = (Quantifies a,Quantifies b)
  quantify (a,b) = pairQ (quantify a) (quantify b)

instance (Quantifiable a, Quantifiable b, Quantifiable c) => Quantifiable (a,b,c) where
  type Quantifies (a,b,c) = (Quantifies a, Quantifies b, Quantifies c)
  quantify (a,b,c) = mapQ (to,from) (quantify a `pairQ` (quantify b `pairQ` quantify c))
    where to (a,(b,c)) = (a,b,c)
          from (a,b,c) = (a,(b,c))

instance Quantifiable a => Quantifiable [a] where
  type Quantifies [a] = [Quantifies a]
  quantify [] = Quantification (return []) null (const [])
  quantify (a:as) =
    (mapQ (to,from) $ pairQ (quantify a) (quantify as))
    `whereQ` (not . null)
    where to (x,xs) = x:xs
          from (x:xs) = (x,xs)

validQuantification q =
  forAllShrink (genQ q) (shrinkQ q) $ isaQ q


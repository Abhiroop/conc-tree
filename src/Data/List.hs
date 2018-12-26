{-# LANGUAGE TypeSynonymInstances #-}
module Data.List where

import Data.Maybe
import Data.Monoid
import Data.Internal.Tree

import Prelude hiding (head, tail, foldMap, length)

import GHC.Conc


-- XXX: Can Conc be Foldable?

type List a = Tree a

instance Ord a => Semigroup (List a) where
  (<>) = conc

instance Ord a => Monoid (List a) where
  mempty = E
  mappend = (<>)

--------------- Predicates-------------

null :: List a -> Bool
null E = True
null _ = False

singleton :: List a -> Bool
singleton E     = False
singleton (S _) = True
singleton _     = False

--------------------------------------

cons :: Ord a => a -> List a -> List a
cons = insert

conc :: Ord a => List a -> List a -> List a
conc E E     = E
conc E (S a) = S a
conc E (C c l r) = C c l r
conc (S a) E = S a
conc (S a) (S b) = insert a (S b)
conc (S a) (C c l r) = insert a (C c l r)
conc (C c l r) E = C c l r
conc (C c l r) (S a) = balance B (C c l r) (S a)
conc (C c1 l1 r1) (C c2 l2 r2) = balance B (C c1 l1 r1) (C c2 l2 r2)

{-
list == S
item == remove S

list :: a -> List a
list x = S x

item :: List a -> a
item (S x) = x
item _ = error "sorry partial"

-- Theorems
list . item = id
conc (left xs) (right xs) = xs
split xs conc = xs

-}

head :: List a -> Maybe a
head E = Nothing
head (S x) = Just x
head (C _ l _) = head l

tail :: Ord a => List a -> Maybe (List a)
tail E = Nothing
tail (S x) = Nothing
tail (C _ l r)
  = let left_tail = fromMaybe E (tail l)
     in Just $ conc left_tail r


addleft :: Ord a => a -> List a -> List a
addleft x E = S x
addleft x xs@(S _) = conc (S x) xs
addleft x (C _ ys zs) = conc (addleft x ys) zs

addright :: Ord a => List a -> a -> List a
addright E x = S x
addright xs@(S _) x = conc xs (S x)
addright (C _ ys zs) x = conc ys (addright zs x)

{-
map
reduce
length
filter
reverse
-}

foldMap :: Monoid m
          => (a -> m)
          -> List a -- the actual structure
          -> m
foldMap _ E = mempty
foldMap f (S x) = f x
foldMap f (C _ ys zs) = par2 mappend (foldMap f ys) (foldMap f zs)

par2 :: (a -> b -> c) -> a -> b -> c
par2 f x y = x `par` y `par` f x y


length :: List a -> Int
length = getSum . foldMap (\_ -> Sum 1)


last :: Ord a => List a -> Maybe a
last xs = go xs
  where
    go l = let penult = head l
            in case penult of
                 Nothing -> Nothing
                 (Just l') -> let z = tail l
                              in case z of
                                   Nothing -> Just l'
                                   otherwise -> go (fromMaybe E z)

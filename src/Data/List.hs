{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.List
 ( List (..)
 , cons
 , empty
 , head
 , snoc
 , tail

 ----- Predicates----
 , null
 , singleton

 ----- Operations----
 , filter
 , foldMap
 , length
 , map
 , reverse
 )
where

import Control.Concurrent.Utils
import GHC.Conc
import Data.Maybe
import Data.Monoid
import Data.Internal.Tree

import Prelude hiding ( head, tail, foldMap, length
                      , reverse, last, map, null
                      , filter)


-- XXX: Can conc List be Foldable? Yes with a newtype wrapper
type List a = Tree a

empty :: List a
empty = E

instance Ord a => Semigroup (List a) where
  (<>) = conc

instance Ord a => Monoid (List a) where
  mempty = E
  mappend = (<>)

instance Show a => Show (List a) where
  show t = "C " ++ go t
    where
      go E = "E"
      go (S x) = show x
      go (C _ _ l r) = go l ++ "," ++ go r

parallelDepth = 4 -- a magic number

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
conc E (C h c l r) = C h c l r
conc (S a) E = S a
conc (S a) (S b) = insert a (S b)
conc (S a) (C h c l r) = insert a (C h c l r)
conc (C h c l r) E = C h c l r
conc (C h c l r) (S a) = balance (1 + h) B (C h c l r) (S a)
conc (C h1 c1 l1 r1) (C h2 c2 l2 r2) =
  balance (1 + max h1 h2) B (C h1 c1 l1 r1) (C h2 c2 l2 r2)


head :: List a -> Maybe a
head E = Nothing
head (S x) = Just x
head (C _ _ l _) = head l

tail :: Ord a => List a -> Maybe (List a)
tail E = Nothing
tail (S x) = Nothing
tail (C _ _ l r)
  = let left_tail = fromMaybe E (tail l)
     in Just $ conc left_tail r


snoc :: Ord a => List a -> a -> List a
snoc E x = S x
snoc xs@(S _) x = conc xs (S x)
snoc (C _ _ ys zs) x = conc ys (snoc zs x)


-- When you want to redefine the mempty and mappend operation
foldMap' :: (a -> m)
         -> (m -> m -> m)
         -> m
         -> List a -- the actual structure
         -> m
foldMap' f g unit t = go t 0
  where
    go E _ = unit
    go (S x) _ = f x
    go (C h _ !ys !zs) n
      | n <= parallelDepth = gol `par` gor `par` g gol gor
      | otherwise = g gol gor
      where
        gol = go ys (n + 1)
        gor = go zs (n + 1)

foldMap :: Monoid m
          => (a -> m)
          -> List a -- the actual structure
          -> m
foldMap f t = go t 0
  where
    go E _ = mempty
    go (S x) _ = f x
    go (C h _ !ys !zs) n
      | n <= parallelDepth = gol `par` gor `par` mappend gol gor
      | otherwise = mappend gol gor
      where
        gol = go ys (n + 1)
        gor = go zs (n + 1)

map :: Ord b => (a -> b) -> List a -> List b
map f xs = foldMap (\x -> S (f x)) xs

length :: List a -> Int
length = getSum . foldMap (\_ -> Sum 1)

reverse :: Ord a => List a -> List a
reverse = foldMap' S (\ys zs -> conc zs ys) E

last :: Ord a => List a -> Maybe a
last = head . reverse

fold :: Monoid m => List m -> m
fold = foldMap id

filter :: Ord a => (a -> Bool) -> List a -> List a
filter p = foldMap (\x -> if p x then (S x) else E)

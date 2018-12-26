module Data.Internal.Tree where

{- Previously

data Conc a =
    Empty
  | Singleton a -- store final array here
  | Concat (Conc a) (Conc a) --enrich this with more info -- the bit partition - thats for lookup not imp
                             --enrich with height info -- imp for balancing
                             --enrich with level info for concurrency
  deriving (Show, Eq)
-}



data Color = R | B deriving Show

data Tree a
  = E
  | S a
  | C Color (Tree a) (Tree a)
  deriving Show

{-
Inv 1. No red node has a red parent.
Inv 2. Every path from the root to an empty node contains the same number
of black nodes.
-}
insert :: Ord a => a -> Tree a -> Tree a
insert x xs = makeBlack $ ins xs
  where
    ins E = S x
    ins (S a) = C R (S x) (S a)
    ins (C c l r) = balance c (ins l) r -- always traverse left and trust the balancing

    makeBlack (C _ l r) = C B l r
    makeBlack a = a

balance :: Color -> Tree a -> Tree a -> Tree a

balance B (C R (C R a b) c) d = C R (C B a b) (C B c d)
balance B (C R a (C R b c)) d = C R (C B a b) (C B c d)
balance B a (C R (C R b c) d) = C R (C B a b) (C B c d)
balance B a (C R b (C R c d)) = C R (C B a b) (C B c d)
balance color a b = C color a b

---------Steele's Accessors------------

left :: Tree a -> Tree a
left E         = E
left (S _) = E
left (C _ l _) = l

right :: Tree a -> Tree a
right E         = E
right (S _) = E
right (C _ _ r)  = r

split :: Tree a -> (Tree a -> Tree a -> Tree a) -> Tree a
split E _         = E
split (S _) _ = E
split (C _ l r) f  = f l r

---------------------------------------

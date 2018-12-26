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
type Height = Int

data Color
  = R
  | B
  deriving (Show)

data Tree a
  = E
  | S a
  | C Height
      Color
      (Tree a)
      (Tree a)
  deriving (Show)

height :: Tree a -> Int
height E = 0
height (S _) = 0
height (C h _ _ _) = h

{-
Inv 1. No red node has a red parent.
Inv 2. Every path from the root to an empty node contains the same number
of black nodes.
-}
insert :: Ord a => a -> Tree a -> Tree a
insert x xs = makeBlack $ ins xs
  where
    ins E = S x
    ins (S a) = C 1 R (S x) (S a)
    ins (C h c l r) = balance h c (ins l) r -- always traverse left and trust the balancing
    makeBlack (C h _ l r) = C h B l r
    makeBlack a = a

balance :: Height -> Color -> Tree a -> Tree a -> Tree a
balance h B (C h1 R (C h2 R a b) c) d =
  let h_child = 1 + max (height c) (height d)
      h_parent = 1 + max h2 h_child
  in C h_parent R (C h2 B a b) (C h_child B c d)
balance h B (C h1 R a (C h2 R b c)) d =
  let h_child_1 = 1 + max (height a) (height b)
      h_child_2 = 1 + max (height c) (height d)
      h_parent = 1 + max h_child_1 h_child_2
  in C h_parent R (C h_child_1 B a b) (C h_child_2 B c d)
balance h B a (C h1 R (C h2 R b c) d) =
  let h_child_1 = 1 + max (height a) (height b)
      h_child_2 = 1 + max (height c) (height d)
      h_parent = 1 + max h_child_1 h_child_2
  in C h_parent R (C h_child_1 B a b) (C h_child_2 B c d)
balance h B a (C h1 R b (C h2 R c d)) =
  let h_child = 1 + max (height a) (height b)
      h_parent = 1 + max h2 h_child
  in C h_parent R (C h2 B a b) (C h_child B c d)
balance _ color a b = C (1 + max (height a) (height b)) color a b

---------Steele's Accessors------------
left :: Tree a -> Tree a
left E = E
left (S _) = E
left (C _ _ l _) = l

right :: Tree a -> Tree a
right E = E
right (S _) = E
right (C _ _ _ r) = r

split :: Tree a -> (Tree a -> Tree a -> Tree a) -> Tree a
split E _ = E
split (S _) _ = E
split (C _ _ l r) f = f l r
---------------------------------------

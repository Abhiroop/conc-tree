{-# LANGUAGE BangPatterns #-}
module Data.Internal.TreeNAry where

import Debug.Trace

type Height = Int

-- A conc-rose tree
data NTree a
  = E
  | S a
  | C !Height !Info [NTree a]
  deriving Show

data Info
  = F
  | U
  deriving Show

leftmost_length :: NTree a -> Int
leftmost_length (C _ _ x@((S _):_)) = length x
leftmost_length (C _ _ (x@(C _ _ _ ):_)) = leftmost_length x

-- insert :: a -> NTree a -> NTree a
-- insert x E = S x
-- insert x (S a) = C 2 [(S x), (S a)]
-- insert x node@(C n t@(h:_))
--   | n < 8 = case h of
--               (S _) -> C (n + 1) ((S x):t)
--               otherwise -> insert x h --C (n + 1) ((S x):t)
--   | otherwise = let foo = C 1 [(S x)]
--                  in C 2 [foo,node] --C 2 [(S x),node]

-- insert :: a -> NTree a -> NTree a
-- insert x E = (S x)
-- insert x (S a) = C 2 [(S x), (S a)]
-- insert x node@(C n (t:ts))
--   | (n < 8) || size t < 8 = C n (insert x t : ts)
--   | otherwise = C 2 [(S x),node]

-- l = go 9 E
--   where
--     go 0 !t = insert 0 t
--     go n !t = go (n - 1) (insert n t)

createBranch :: a -> Height -> NTree a
createBranch x 0 = (S x)
createBranch x h = C h U [(createBranch x (h-1))]

branching_factor = 4

insert :: a -> NTree a -> NTree a
insert x E = S x
insert x (S a) = C 1 U [(S x), (S a)]
insert x (C h F ts)
  = let branch = createBranch x h
     in C (h + 1) U (branch:ts)
insert x (C h U foo@(t@(C _ U _):ts))
  = C h U ((insert x t):ts)
insert x (C h U foo@(t@(C h2 F _):ts))
  = let branch = createBranch x h2
     in C h U (branch:foo)
insert x (C h U foo@((S _):ts))
  = if length foo == branching_factor
    then let branch = createBranch x h
          in C (h + 1) U [branch,(C h F foo)]
    else C h U ((S x):foo)

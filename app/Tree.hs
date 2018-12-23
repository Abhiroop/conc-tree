module Tree where

import Prelude hiding (head, tail)
import Data.Maybe

data Conc a =
    Empty
  | Singleton a -- store final array here
  | Concat (Conc a) (Conc a) --enrich this with more info -- the bit partition
  deriving (Show, Eq)

rebalance :: Conc a -> Conc a
rebalance = undefined


-- persistent bit-partitioned vector trie


-- XXX: Can Conc be Foldable?

type List a = Conc a


--------------- Predicates-------------

null :: List a -> Bool
null Empty = True
null _     = False

singleton :: List a -> Bool
singleton Empty         = False
singleton (Singleton _) = True
singleton _             = False

--------------------------------------

---------------- Accessors------------

left :: List a -> List a
left Empty         = Empty
left (Singleton _) = Empty
left (Concat l _)  = l

right :: List a -> List a
right Empty         = Empty
right (Singleton _) = Empty
right (Concat _ r)  = r

split :: List a -> (List a -> List a -> List a) -> List a
split Empty _         = Empty
split (Singleton _) _ = Empty
split (Concat l r) f  = f l r

---------------------------------------

conc :: List a -> List a -> List a
conc xs ys = Concat xs ys

{-
list == Singleton
item == remove Singleton

list :: a -> List a
list x = Singleton x

item :: List a -> a
item (Singleton x) = x
item _ = error "sorry partial"

-- Theorems
list . item = id
conc (left xs) (right xs) = xs
split xs conc = xs

-}

head :: List a -> Maybe a
head Empty = Nothing
head (Singleton x) = Just x
head (Concat l _) = head l

tail :: List a -> Maybe (List a)
tail Empty = Nothing
tail (Singleton x) = Nothing
tail (Concat l r)
  = let left_tail = fromMaybe Empty (tail l)
     in Just $ append left_tail r -- Why do we use (append left_tail r) here instead of just (Concat left_tail r)


append :: List a -> List a -> List a
append Empty ys = ys
append xs Empty = xs
append xs ys = rebalance (conc xs ys)

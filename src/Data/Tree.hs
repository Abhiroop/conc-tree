module Data.Tree where

import Prelude hiding (head, tail, foldMap)
import Data.Maybe

data Conc a =
    Empty
  | Singleton a -- store final array here
  | Concat (Conc a) (Conc a) --enrich this with more info -- the bit partition - thats for lookup not imp
                             --enrich with height info -- imp for balancing
                             --enrich with level info for concurrency
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

addleft :: a -> List a -> List a
addleft x Empty = Singleton x
addleft x xs@(Singleton _) = append (Singleton x) xs
addleft x (Concat ys zs) = append (addleft x ys) zs

addright :: List a -> a -> List a
addright Empty x = Singleton x
addright xs@(Singleton _) x = append xs (Singleton x)
addright (Concat ys zs) x = append ys (addright zs x)

{-
map
reduce
length
filter
reverse
-}

foldMap :: ParallelMonoid m
          => (a -> m)
          -> List a -- the actual structure
          -> m
foldMap _ Empty = par_mempty
foldMap f (Singleton x) = f x
foldMap f (Concat ys zs) = par_mappend (foldMap f ys) (foldMap f zs) -- parallelism hidden here

foldMap' :: (a -> m)
         -> (m -> m -> m)
         -> m
         -> List a -- the actual structure
         -> m
foldMap' _ _ unit Empty = unit
foldMap' f _ _ (Singleton x) = f x
foldMap' f g unit (Concat ys zs) = g (foldMap' f g unit ys) (foldMap' f g unit zs) -- parallelism hidden here

length :: List a -> Int
length xs = foldMap' (\x -> 1) (+) 0 xs

class ParallelMonoid m where
  par_mempty :: m
  par_mappend :: m -> m -> m


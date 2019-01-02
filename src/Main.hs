{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.List as L

-- l = go 1000000 L.empty
--   where
--     go 0 t = L.cons 0 t
--     go n t = go (n - 1) (L.cons n t)

l = go 10000000 L.empty
  where
    go 0 !t = L.cons 0 t
    go n !t = go (n - 1) (L.cons n t)

foo = go 10 L.empty
  where
    go 0 t = t
    go n t = go (n - 1) (L.conc (L.single n) t)





len :: [Int] -> Int
len = foldr (\_ c -> c + 1) 0

main :: IO ()
main = print $ L.head l
-- main = print $ S.length $ S.fromList [10000000,9999999..0]

module Main where

import qualified Data.List as L


l = go 10000000 L.empty
  where
    go 0 l = L.cons 0 l
    go n l = L.cons n $ go (n - 1) l


main :: IO ()
main = print $ L.length l

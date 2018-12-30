module Control.Concurrent.Utils where

import GHC.Conc

par2 :: (a -> b -> c) -> a -> b -> c
par2 f x y = x `par` y `par` f x y

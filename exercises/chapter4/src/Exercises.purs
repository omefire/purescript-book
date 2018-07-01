module Exercises where

import Prelude

isEven :: Int -> Boolean
isEven n | n == 1 = false
       | n == -1 = false
isEven n | n == 2 = true
       | n == -2 = true
isEven n | n < 0 = isEven $ (n + 2)
         | otherwise = isEven $ (n - 2)

module Exercises where

import Prelude

import Data.Array (uncons)
import Data.Maybe

isEven :: Int -> Boolean
isEven n | n == 1 = false
       | n == -1 = false
isEven n | n == 2 = true
       | n == -2 = true
isEven n | n < 0 = isEven $ (n + 2)
         | otherwise = isEven $ (n - 2)

countEven :: Array Int -> Int
countEven xs = countEvenHelper xs 0
 where
  countEvenHelper xs count = case uncons xs of 
                                    Just { head: x, tail: xss } -> case (isEven x) of
                                                                    true -> countEvenHelper xss (count + 1)
                                                                    false -> countEvenHelper xss count
                                    Nothing -> count

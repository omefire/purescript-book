module Exercises where

import Prelude

import Data.Array (uncons, filter, (..), length, (!!))
import Data.Maybe
import Data.Ring (negate)
import Control.MonadZero (guard)
import Data.Tuple (Tuple(..))

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

squares :: Array Int -> Array Int
squares xs = map (\x -> x * x) xs

removeNegativeNumbers :: Array Int -> Array Int
removeNegativeNumbers xs = filter (\x -> x > 0) xs

infixr 5 filter as <$?>

factors n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure $ Tuple x y

triples :: Int -> Array (Array Int)
triples n = do
    x <- 1..n
    y <- 1..n
    z <- 1..n
    guard $ (x * x) + (y * y) == (z * z)
    pure $ [x, y, z]

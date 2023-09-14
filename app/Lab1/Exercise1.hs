module Lab1.Exercise1 where

import Control.Monad
import Data.Char
import Data.List
import GHC.Float
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.State

-- Exercise 1
-- A generator which only allows for natural numbers to be generates
genPositiveNumbers :: Gen Integer
genPositiveNumbers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>= 0)

-- https://www.wikiwand.com/en/Factorial
factorial :: Integer -> Integer 
factorial n | n == 0 = 1
            | n >= 0 = n * factorial (n - 1)
            | otherwise = error "n is not allowed to be negative"

isDivisableByX :: Integer -> Integer -> Bool 
isDivisableByX n x = n `mod` x == 0

prop_divisableBySmallerThanN :: Integer -> Bool 
prop_divisableBySmallerThanN n = all (isDivisableByX (factorial n)) [1..n]

prop_factorialBase :: Bool 
prop_factorialBase = factorial 0 == 1

prop_factorialIncrement :: Integer -> Bool 
prop_factorialIncrement n = factorial n * (n+1) == factorial (n+1)

main :: IO ()
main = do
  quickCheck prop_factorialBase
  quickCheck prop_divisableBySmallerThanN
  quickCheck (forAll genPositiveNumbers prop_factorialIncrement)

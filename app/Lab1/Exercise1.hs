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
-- https://www.wikiwand.com/en/Factorial
factorial :: Integer -> Integer 
factorial 0 = 1 
factorial n = n * factorial (n-1)

main :: IO ()
main = do
  print $ factorial 10

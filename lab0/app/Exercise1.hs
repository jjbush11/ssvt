module Exercise1 where

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
genPositiveNumbers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

-- A generator which generates numbers smaller or equal to zero
genNegativeNumbers :: Gen Integer
genNegativeNumbers = arbitrary `suchThat` (<= 0)

-- A generic function which allows to generate a list of powers of a given power
sumOfPowers :: Integer -> Integer -> Integer
sumOfPowers power n
  | n <= 0 = error "n must be positive"
  | otherwise = sum [x ^ power | x <- [1 .. n]]

-- Property for sum of squares:
-- 1ˆ2 + 2^2 + 3^2 + ... + n^2 = (n(n+1)(2n+1))/6
prop_squareSum :: Integer -> Bool
prop_squareSum n = sumOfPowers 2 n == (n * (n + 1) * (2 * n + 1)) `div` 6

-- Property for sum of power of three's:
-- 1ˆ3 + 2^3 + 3^3 + ... + n^3 = ((n(n+1))/2)ˆ2
prop_powerOfThreeSum :: Integer -> Bool
prop_powerOfThreeSum n = sumOfPowers 3 n == ((n * (n + 1)) `div` 2) ^ 2

main :: IO ()
main = do
  -- Test the properties
  -- Test both properties with the genPositiveNumbers generator and the genNegativeNumbers generator
  -- Expect failure on negative numbers
  quickCheck (forAll genPositiveNumbers prop_squareSum)
  quickCheck $ expectFailure $ forAll genNegativeNumbers prop_squareSum
  quickCheck (forAll genPositiveNumbers prop_powerOfThreeSum)
  quickCheck $ expectFailure $ forAll genNegativeNumbers prop_powerOfThreeSum

-- Time spent: 1 hour, of which a lot with making the dependencies work
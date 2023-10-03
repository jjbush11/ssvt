module Exercise3 where

import Test.QuickCheck
import Lecture3 (p)

-- Exercise 3
-- Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties
calculateMinimalPropertySubsets :: [a -> Integer -> Bool] -> (Integer -> a) -> [[a -> Integer -> Bool]]
calculateMinimalPropertySubsets properties functionUnderTest = undefined

main :: IO ()
main = do
  print "Hello World"
module Exercise5 where

import Test.QuickCheck

-- Exercise 5
-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose cases are subsets of other properties, etc.
calculateConjectures :: [Integer -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([a -> Integer -> Bool], [a -> Integer -> Bool])]
calculateConjectures mutators nMutants props fut = undefined

main :: IO ()
main = do
  print "Hello World"
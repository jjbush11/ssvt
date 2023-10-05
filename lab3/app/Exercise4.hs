module Exercise4 where

import Test.QuickCheck
import Exercise5 (calculateConjectures)

-- Exercise 4
-- Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill
calculateStrengthOfProperties :: [a -> Integer -> Bool] -> (Integer -> a) -> [Float]
calculateStrengthOfProperties = undefined

main :: IO ()
main = do
  print "Hello World"
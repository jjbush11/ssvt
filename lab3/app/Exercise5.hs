module Exercise5 where

import Test.QuickCheck

-- Exercise 5
-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose cases are subsets of other properties, etc.
-- Not sure if the type definition is correct
-- I have the feeling that it should be dependant on a function under test
-- In that case the type definition should be something like:
-- calculateConjectures :: [a -> Integer -> Bool] -> (Integer -> a) -> [(a -> Integer -> Bool, a -> Integer -> Bool)]
calculateConjectures :: [a -> Integer -> Bool] -> [(a -> Integer -> Bool, a -> Integer -> Bool)]
calculateConjectures = undefined 

main :: IO ()
main = do
  print "Hello World"
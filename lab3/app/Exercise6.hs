module Exercise6 where

import Test.QuickCheck

-- Exercise 6 (Bonus)
-- Create a function that we pass the function under test, a set of properties, and a number
-- indicating the number of mutants, that visualizes the results of the functions from the previous exercises.
visualizeResults :: [a -> Integer -> Bool] -> (Integer -> a) -> Integer -> String
visualizeResults = undefined

main :: IO ()
main = do
  print "Hello World"
module Exercise2 where

import Test.QuickCheck

-- Exercise 2
-- Where the first argument is the number of mutants (4000 in the FitSpec example), the second
-- argument is the list of properties, and the third argument is the function under test (the
-- multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- Document the effect of which mutations are used and which properties are used on the number of survivors.
countSurvivors :: Integer -> [[Integer] -> Integer -> Property] -> (Integer -> [Integer]) -> Integer
countSurvivors = undefined

main :: IO ()
main = do
  print "Hello World"
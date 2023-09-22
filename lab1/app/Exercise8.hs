module Exercise8 where

import Lecture1
import Test.QuickCheck

-- The function makes use of Haskell's lazy evaluation to create an infinite long list
-- One is able to get e.g. the first 10 counterexamples by using take 10 counterExamples
counterExamples :: [([Integer], Integer)]
counterExamples = counterExamplesFrom 2

-- A recursive function which calculates the product of consecutive primes
-- If the product of those primes + 1 is not prime, it should be added to the list.
-- Because it keeps on adding counterexamples, the list can become infinitely long.
counterExamplesFrom :: Int -> [([Integer], Integer)]
counterExamplesFrom n
  | not $ prime productPlusOne = (primeList, productPlusOne) : counterExamplesFrom (n + 1)
  | otherwise = counterExamplesFrom (n + 1)
  where
    primeList = take n primes
    productPlusOne = product primeList + 1

prop_isCounterExample :: ([Integer], Integer) -> Bool
prop_isCounterExample (primeList, productPlusOne) = product primeList + 1 == productPlusOne && not (prime productPlusOne)

-- Exercise 8
main :: IO ()
main = do
  -- We have chosen to calculate the first 12 counterexamples.
  -- The reason for that being that a higher number took to long to calculate.
  -- The first 12 counterexamples are:
  let calculatedCounterExamples = take 12 counterExamples
  -- We test if the counterexamples are indeed counterexamples.
  quickCheck (forAll (elements calculatedCounterExamples) prop_isCounterExample)
  -- What we didn't understand is why QuickCheck would output the following:
  -- +++ OK, passed 100 tests.
  -- This is probably because we are using the forAll function, which takes a random element from the list.
  -- We would expect QuickCheck to output something like:
  -- +++ OK, passed 12 tests.
  -- Due to that we resorted to using the mapM_ function to test all the counterexamples instead. 
  -- This ensures that all the counterexamples are tested.
  -- The result is that there are twelf individual tests, which all pass.
  -- This suggests that the counterexamples list contains correct counterexamples.
  -- Unfortunately, this doesn't prove that there might be an incorrect counterexample in the list.
  -- We would have to resort to a mathematical proof to prove that.
  mapM_ (\ex -> quickCheck (property $ prop_isCounterExample ex)) calculatedCounterExamples

-- Time spent: 1 hour

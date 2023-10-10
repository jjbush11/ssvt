module Exercise4 where

import Control.Monad
import Exercise1
import Exercise2
import Exercise3
import FitSpec (testFitSpec)
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- Exercise 4
-- Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill. 
-- It was the idea to retrieve the score from the PropertyAnalysis record, but this gave a: labs: Prelude.head: empty list error, which we couldn't fix.
-- Therefore it is now implemented as follows:
calculateStrengthOfProperties :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Float
calculateStrengthOfProperties mutators nMutants properties fut = do
  -- We first execute the executeMutation function to get the raw results
  mutationResult <- executeMutation mutators nMutants properties fut

  -- To determine the strength of a set of properties, we need to calculate the percentage of mutants they kill.
  -- We first determine the total number of mutants, by taking the length of the result list.
  let totalMutants = fromIntegral $ length mutationResult

  -- We then determine the number killed mutants, by taking the length of the list of killed mutants.
  -- A mutant is killed if a list contains at least one False value.
  let totalKilled = fromIntegral $ length $ filter id $ map (any not) mutationResult

  -- We then calculate the strength by dividing the number of killed mutants by the total number of mutants and multiplying it by 100.
  return (totalKilled / totalMutants * 100)

main :: IO ()
main = do
  let fut = multiplicationTable
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements, anyList, addElements]

  -- Calculate the strength for 1 property
  let properties1 = createSubset [0] multiplicationTableProps
  res1 <- calculateStrengthOfProperties mutators nMutants properties1 fut
  -- Pretty print the result of calculateStrengthOfProperties
  putStrLn $ "The strength of the properties is " ++ show res1 ++ "%"

  -- Calculate the strength for 2 properties
  let properties2 = createSubset [0, 1] multiplicationTableProps
  res2 <- calculateStrengthOfProperties mutators nMutants properties2 fut
  -- Pretty print the result of calculateStrengthOfProperties
  putStrLn $ "The strength of the properties is " ++ show res2 ++ "%"

  -- Calculate the strength for 3 properties
  let properties3 = createSubset [0, 1, 2] multiplicationTableProps
  res3 <- calculateStrengthOfProperties mutators nMutants properties3 fut
  -- Pretty print the result of calculateStrengthOfProperties
  putStrLn $ "The strength of the properties is " ++ show res3 ++ "%"

  -- Calculate the strength for all properties
  let properties4 = multiplicationTableProps
  res4 <- calculateStrengthOfProperties mutators nMutants properties4 fut
  -- Pretty print the result of calculateStrengthOfProperties
  putStrLn $ "The strength of the properties is " ++ show res4 ++ "%"

-- As you would expect the more properties you pass to the calculateStrengthOfProperties function the stronger the set of properties.

-- Indication of time spent: 1.5 hours
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
-- Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill
calculateStrengthOfProperties :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Float
calculateStrengthOfProperties mutators nMutants properties fut = do
  res <- executeMutation mutators nMutants properties fut
  let totalMutants = fromIntegral $ length res

  let survivorsCount = fromIntegral $ length $ filter id $ map and res

  return ((totalMutants - survivorsCount) / totalMutants * 100)

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
module Exercise4 where

import Control.Monad
import Exercise1
import FitSpec (testFitSpec)
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Exercise2
import Exercise3

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
  --let properties = multiplicationTableProps
  let properties = createSubset [0] multiplicationTableProps
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements, anyList, addElements]
  
  res <- calculateStrengthOfProperties mutators nMutants properties fut

  -- Pretty print the result of calculateStrengthOfProperties
  putStrLn $ "The strength of the properties is " ++ show res ++ "%"

  -- Give different examples for different combinations of properties
  -- Documentation
  -- Indication of time spent
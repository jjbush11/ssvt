module Exercise6 where

import Control.Monad
import Data.List (subsequences, transpose)
import Exercise1
import Exercise2
import Exercise3
import Exercise5
import FitSpec (testFitSpec)
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- Exercise 6 (Bonus)
-- Create a function that we pass the function under test, a set of properties, and a number
-- indicating the number of mutants, that visualizes the results of the functions from the previous exercises.
report :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO ()
report mutators nMutants properties fut = do
  -- We first execute the executeMutation function to get the raw results
  mutationResults <- executeMutation mutators nMutants properties fut

  -- We then apply the computeAnalysis function to get a PropertyAnalysis record.
  let transposedResults = transposeRawResults mutationResults
  let analysis = computeAnalysis transposedResults
  
  let conjectures = computeConjectures $ computeAnalyses $ transposeRawResults mutationResults
  let minimalSubsets = computeMinimalPropertySubsets mutationResults

  -- Print report based on the calculated analysis
  putStrLn $ "The total number of mutants is: " ++ show (totalMutants analysis)
  putStrLn $ show (nKilled analysis) ++ " mutants killed in total"
  putStrLn $ show (nSurvivors analysis) ++ " survivors (" ++ show (score analysis) ++ "% killed)"
  putStrLn ""
  putStrLn "The minimal property subsets are: "
  forM_ minimalSubsets print

  -- Print the conjectures: 
  putStrLn "\nThe conjectures are: "
  forM_ conjectures print

  -- We didn't have time to add the minimal subsets to the report, because we then had to change the implementation of Exercise 3.

main :: IO ()
main = do
  -- Define the inputs for the report function
  let fut = multiplicationTable
  let properties = multiplicationTableProps
  let mutator = negateElements
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements] -- , anyList, emptyList]

  -- Print a report
  report mutators nMutants properties fut

-- Time Spent: 0.5 hour
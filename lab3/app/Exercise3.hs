module Exercise3 where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Exercise1
import Exercise2
import Lecture3 (p)
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- Filter out the elements based on the given indices
createSubset :: [Int] -> [a] -> [a]
createSubset indices = map snd . filter (\(index, _) -> index `elem` indices) . zip [0 ..]

-- Function to check if the first list is a proper subset of the second list
isProperSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isProperSubsetOf xs ys = xs `isSubsetOf` ys && length xs < length ys
  where
    -- Function to check if a list is a subset of another list
    isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsetOf xs ys = all (`elem` ys) xs

-- Function to filter out non-minimal subsets from a list of subsets
filterMinimalSubsets :: (Eq a) => [[a]] -> [[a]]
filterMinimalSubsets subsets = filter isMinimal subsets
  where
    isMinimal subset = all (not . (`isProperSubsetOf` subset)) subsets

-- The main function to compute the list of PropertyAnalysis records.
computeAnalyses :: [(Int, [Bool])] -> [PropertyAnalysis]
computeAnalyses rawResult = [computeAnalysis subset | subset <- tail . subsequences $ rawResult] -- Exclude empty subset

-- Exercise 3
-- A function that calculates the minimal property subsets, given a 'function under test' and a set of properties

-- So the idea around this task is to find the minimal property subsets.
-- The original approach we used was as follows:
-- 1. Generate all property subsets, by making use of subsequences.
-- Due to this approach, there will be 2^n subsets, where n is the number of properties.
-- For the case of multiplicationTable, this means 2^5 = 32 - 1 (the empty set) subsets.
-- Due to this exponential increase in subsets, the program might take a while to run if the number of properties is increased.
-- 2. For each subset, use the countSurvivors function to determine how many mutants survive when tested against that subset.
-- 3. Rank the subsets by effectiveness
-- 4. Among these subsets, identify one that is both minimal in size and most effective at killing mutants

-- However, this approach seemed rather inefficient, because it would calculate the number of survivors for each subset.
-- But the results would then not be comparible, because the total of valid mutants might be different for each subset.
-- Thus we decided to refactor the code from Exercise 2, to be able to inferre it from the result of then created executeMutation function.
-- The result of the executeMutation function is then used to get the analysis for all possible subsets.
-- The analysis for all possible subsets is then filtered to only include the minimal subsets.
calculateMinimalPropertySubsets :: [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[PropertyId]]
calculateMinimalPropertySubsets mutators properties functionUnderTest = do
  -- We first execute the executeMutation function to get the raw results
  mutationResults <- executeMutation mutators 4000 properties functionUnderTest

  -- Get the list of propertyAnalysis records,
  -- where each record contains:
  -- \* the subset of properties,
  -- \* the raw results,
  -- \* the total number of mutants,
  -- \* the number of survivors,
  -- \* the number of killed mutants,
  -- \* the score,
  -- \* the mutants that survived and the mutants that were killed
  let analyses = computeAnalyses $ transposeRawResults mutationResults

  -- Get the analysis for the total set of properties
  let totalAnalysis = head $ filter (\analysis -> propertyIndices analysis == Set.fromList [0 .. length properties - 1]) analyses

  -- For the totalAnalysis, retrieve the amount of survivors
  let totalAnalysisSurvivors = nSurvivors totalAnalysis

  -- Filter out the propertyAnalyses that have the same amount of survivors as the totalAnalysis
  -- By doing this, instead of checking if the amount of survivors is equal to zero,
  -- we take into account the situation where all the properties don't kill all the mutants.
  let survivingAnalyses = filter (\analysis -> nSurvivors analysis == totalAnalysisSurvivors) analyses

  -- For every propertyAnalysis, which have the same amount of survivors as the totalAnalysis, retrieve the indices
  let survivingAnalysesPropertyIndices = map (Set.toList . propertyIndices) survivingAnalyses

  -- Filter out the propertyAnalyses that are not minimal
  let minimalPropertySubsets = filterMinimalSubsets survivingAnalysesPropertyIndices

  return minimalPropertySubsets

main :: IO ()
main = do
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements]
  minimalPropertySubsets <- calculateMinimalPropertySubsets mutators multiplicationTableProps multiplicationTable
  print "The minimal property subsets are: "
  forM_ minimalPropertySubsets print

-- The result of running this program is:
-- "The minimal property subsets are: "
-- [0,1,3]
-- [2,3]
-- This means that these subsets have the same amount of survivors as the total set of properties, and are minimal.

-- Time spent: 8 hours
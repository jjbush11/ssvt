{-# LANGUAGE InstanceSigs #-}

module Exercise3 where

import Control.Monad
import Data.List
import Exercise1
import Exercise2
import Lecture3 (p)
import MultiplicationTable
import Mutation
import Test.QuickCheck

data Implication = Implication
  { antecedent :: [Int],
    consequent :: [Int]
  }
  deriving (Eq)

instance Show Implication where
  show :: Implication -> String
  show (Implication antecedent consequent) = show antecedent ++ " → " ++ show consequent

data WeightedImplication = WeightedImplication
  { implication :: Implication,
    weight :: Int
  }
  deriving (Show)

-- Filter out the elements based on the given indices
createSubset :: [Int] -> [a] -> [a]
createSubset indices = map snd . filter (\(index, _) -> index `elem` indices) . zip [0 ..]

-- Determine if propA is a subset of propB
isSubsetOf :: [Bool] -> [Bool] -> Bool
isSubsetOf resultsA resultsB = and $ zipWith (-->) resultsA resultsB

hasSurvived :: [Bool] -> Bool
hasSurvived = and

-- Exercise 3
-- Implement a function that calculates the minimal property subsets, given a 'function under test' and a set of properties

-- So the idea around this task is to find the minimal property subsets.
-- The original approach we used was as follows:
-- 1. Generate all property subsets, by making use of subsequences.
-- Due to this approach, there will be 2^n subsets, where n is the number of properties.
-- For the case of multiplicationTable, this means 2^5 = 32 - 1 (the empty set) subsets.
-- Due to this exponential increase in subsets, the program might take a while to run if the number of properties is increased.
-- 2. For each subset, use the countSurvivors function to determine how many mutants survive when tested against that subset.
-- 3. Rank the subsets by effectiveness
-- 4. Among thes subsets, identify one that is both minimal in size and most effective at killing mutants
--
-- However, this approach seemed rather inefficient, because it would calculate the number of survivors for each subset.
-- But the results would then not be comparible, because the total of valid mutants might be different for each subset.s
-- Thus we decided to refactor the code from Exercise 2, to be able  to inferre it from the result of the executeMutation function.
-- calculateMinimalPropertySubsets :: [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[[Integer] -> Integer -> Bool]]
calculateMinimalPropertySubsets mutators properties functionUnderTest = do
  -- For each subset, use the countSurvivors function to determine how many mutants survive when tested against the subset.
  -- Return tuples of the subset and the number of survivors
  mutationResults <- executeMutation mutators 4000 properties functionUnderTest

  -- For all subsets, apply filterProperties to filter out the properties that are not in the subset
  let filteredSubsets = [(subset, map (createSubset subset) mutationResults) | subset <- allPropertiesSubsets]

  -- For each subset, determine if the mutants survive or not
  let survivors = [(propertySubset, map hasSurvived results) | (propertySubset, results) <- filteredSubsets]

  let implications = [(subsetA, subsetB) | (subsetA, resultsA) <- survivors, (subsetB, resultsB) <- survivors, subsetA /= subsetB, resultsA `isSubsetOf` resultsB]

  print implications

  -- Determine implication between subsets of properties

  -- Count the number of survivors for each subset
  let survivorsCount = [(subset, length results) | (subset, results) <- survivors]
  print $ map snd survivorsCount
  
    -- Sort the subsets by the number of survivors and the size of the subset ascending
    -- let sortedSubsets = sortBy (\(subset1, survivors1) (subset2, survivors2) -> compare (length subset1, survivors1) (length subset2, survivors2)) survivorsCount

    -- Determine if a subset is a subset of another subset for each subset
    -- This is done by using the isSubsetOf function and the filteredSubsets
    -- The result is a list of lists of integers, where each inner list represents a set of property indices that form an equivalence class with the same surviving mutants.
  where 
    propertieNumbers = [0 .. length properties - 1]
    propertiesMap = zip propertieNumbers properties
    -- Generate all subsets, excluding the empty set
    -- The empty set is excluded because that doesn't aid us in finding the minimal property subsets
    allPropertiesSubsets = tail $ subsequences propertieNumbers

-- iss: A list of lists of integers, where each inner list represents
-- a set of property indices that form an equivalence class with the same surviving mutants.

-- Which has more priority? as subset with (say) 5% more survivors but 20% less properties,
-- or a subset with 5% less survivors but 20% more property
-- And which property is stronger and thus which subset of properties is the most minimal subset if there are two or more subsets of the same lenght with the same amount of survivors.

main :: IO ()
main = do
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements]
  hoi <- calculateMinimalPropertySubsets mutators multiplicationTableProps multiplicationTable
  print "Hello World"
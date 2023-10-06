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

calculateWeightOfImplication :: Int -> Int -> Int -> Int
calculateWeightOfImplication totalMutants survivorsA survivorsB = (survivorsA - survivorsB) * 100 `div` totalMutants

-- -- Check if a set of properties implies all other properties
-- impliesAll :: [Implication] -> [Int] -> [Int] -> Bool
-- impliesAll closure props allProps = 
--     let impliedProps = nub $ concat [y | (x, y) <- closure, all (`elem` x) props]
--     in all (`elem` impliedProps) allProps

-- -- Determine minimal property subsets
-- minimalPropertySubsets :: [Implication] -> [Int]
-- minimalPropertySubsets closure = 
--     let allProps = nub $ concatMap fst closure ++ concatMap snd closure
--         propCombinations = sortOn length $ subsequences allProps
--         minimalSubsets = filter (\props -> impliesAll closure props allProps) propCombinations
--     in minimalSubsets

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
-- 4. Among these subsets, identify one that is both minimal in size and most effective at killing mutants
--
-- However, this approach seemed rather inefficient, because it would calculate the number of survivors for each subset.
-- But the results would then not be comparible, because the total of valid mutants might be different for each subset.
-- Thus we decided to refactor the code from Exercise 2, to be able to inferre it from the result of the executeMutation function.
-- calculateMinimalPropertySubsets :: [[Integer] -> Gen [Integer]] -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[[Integer] -> Integer -> Bool]]
calculateMinimalPropertySubsets mutators properties functionUnderTest = do
  -- We first execute the executeMutation function to get the raw results
  mutationResults <- executeMutation mutators 4000 properties functionUnderTest

  let totalMutants = length mutationResults

  -- Create a list of tuples, where each tuple contains a subset of properties and the results of the mutation for that subset
  let propertySubsets = [(subset, map (createSubset subset) mutationResults) | subset <- propertySubsequences]

  -- For each subset, determine if the mutants survive or not
  let subsetSurvivors = [(propertySubset, map hasSurvived results) | (propertySubset, results) <- propertySubsets]

  -- Count the number of survivors for each subset
  let survivorsCount = [(subset, length $ filter id results) | (subset, results) <- subsetSurvivors]
  print $ map snd survivorsCount

  -- Determine implication between subsets of properties
  let implications = [(subsetA, subsetB) | (subsetA, resultsA) <- subsetSurvivors, (subsetB, resultsB) <- subsetSurvivors, subsetA /= subsetB, resultsA `isSubsetOf` resultsB]

  -- print implications
  print propertySubsequences
  where
    propertyNumbers = [0 .. length properties - 1]
    -- Generate all subsets, excluding the empty set
    -- The empty set is excluded because that doesn't aid us in finding the minimal property subsets
    propertySubsequences = tail $ subsequences propertyNumbers

-- iss: A list of lists of integers, where each inner list represents
-- a set of property indices that form an equivalence class with the same surviving mutants.

-- Which has more priority? A subset with (say) 5% more survivors but 20% less properties,
-- or a subset with 5% less survivors but 20% more property.
-- And which property is stronger and thus which subset of properties is the most minimal subset if there are two or more subsets of the same length with the same amount of survivors.

main :: IO ()
main = do
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements]
  hoi <- calculateMinimalPropertySubsets mutators multiplicationTableProps multiplicationTable
  print "Hello World"
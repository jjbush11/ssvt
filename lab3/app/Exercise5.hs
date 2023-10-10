{-# LANGUAGE InstanceSigs #-}

module Exercise5 where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Exercise1
import Exercise2
import Exercise3 hiding (isSubsetOf)
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- This datatype was copied from FitSpec
-- We couldn't come up with a better idea for the datatype, so we decided to use this one.
-- We did have to add the Eq and Ord instances, because we needed to be able to compare Conjectures.
data Conjecture = Conjecture
  { isEq :: Bool,
    isIm :: Bool,
    cleft :: [Int],
    cright :: [Int],
    cscore :: Int,
    cnKilled :: Int,
    cnSurvivors :: Int
  }
  deriving (Eq, Ord)

-- Instance of Show for Conjecture
-- This is to pretty print the Conjecture datatype, in the following form: "{1, 2, 3, 4} →  {5, 6, 7}"
-- It also the score of the conjecture, and whether it is an equivalence or implication.
instance Show Conjecture where
  show :: Conjecture -> String
  show (Conjecture isEq isIm cleft cright cscore cnKilled cnSurvivors) = show cleft ++ " → " ++ show cright ++ " (" ++ show cscore ++ "% killed)" ++ (if isEq then " (equivalence)" else " (implication)")

-- The main function to compute a list of conjectures from a list of PropertyAnalysis.
computeConjectures :: [PropertyAnalysis] -> [Conjecture]
computeConjectures analyses =
  -- Convert the list of analyses to a Set to eliminate duplicates and enable efficient operations
  let analysisSet = Set.fromList analyses
      -- Generate all possible conjectures for each analysis, and union them into a single Set
      allConjectures = Set.unions $ map (conjecturesFor analysisSet) analyses
   in -- Filter out the redundant conjectures from the set of all conjectures
      filterNonRedundant allConjectures

-- Given a set of PropertyAnalyses and a single PropertyAnalysis,
-- generate a set of conjectures by comparing the single analysis to each analysis in the set.
conjecturesFor :: Set.Set PropertyAnalysis -> PropertyAnalysis -> Set.Set Conjecture
conjecturesFor set analysis = Set.fromList $ concatMap (compareAnalyses analysis) (Set.toList set)

-- Compare two PropertyAnalyses and generate a list of conjectures based on their comparison.
-- We make use of catMaybes to filter out the Nothing values.
compareAnalyses :: PropertyAnalysis -> PropertyAnalysis -> [Conjecture]
compareAnalyses a1 a2 = catMaybes [eqConjecture, imConjecture]
  where
    -- Check if the two analyses have the same killed and survived mutants.
    -- If so, create an equivalence conjecture.
    eqConjecture
      | mutantsKilled a1 == mutantsKilled a2 && mutantsSurvived a1 == mutantsSurvived a2 =
          Just $ Conjecture True False (Set.toList $ propertyIndices a1) (Set.toList $ propertyIndices a2) scoreValue nKilled nSurvivors
      | otherwise = Nothing
    -- Check if the survived mutants of the first analysis is a subset of the second analysis.
    -- If so, create an implication conjecture.
    imConjecture
      | Set.isSubsetOf (mutantsSurvived a1) (mutantsSurvived a2) =
          Just $ Conjecture False True (Set.toList $ propertyIndices a1) (Set.toList $ propertyIndices a2) scoreValue nKilled nSurvivors
      | otherwise = Nothing

    -- Compute the score value, number of killed and survived mutants based on the union and intersection of the killed and survived sets.
    scoreValue = (nKilled * 100) `div` (nKilled + nSurvivors)
    nKilled = Set.size (Set.union (mutantsKilled a1) (mutantsKilled a2)) - Set.size (Set.intersection (mutantsKilled a1) (mutantsKilled a2))
    nSurvivors = Set.size (Set.intersection (mutantsSurvived a1) (mutantsSurvived a2))

-- Given a set of all conjectures, filter out those that are redundant.
filterNonRedundant :: Set.Set Conjecture -> [Conjecture]
filterNonRedundant allConjectures = filter (\c -> not (isRedundant c allConjectures)) (Set.toList allConjectures)
  where
    -- Check if a given conjecture is redundant by comparing it with all other conjectures.
    isRedundant :: Conjecture -> Set.Set Conjecture -> Bool
    isRedundant conjecture = any (\other -> implies other conjecture && other /= conjecture) . Set.toList
    -- Check if one conjecture implies another.
    implies :: Conjecture -> Conjecture -> Bool
    implies c1 c2 = (cleft c1 `isSubsetOf` cleft c2) && (cright c1 `isSubsetOf` cright c2)

-- Helper function to check if a list is a subset of another list.
isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

-- Exercise 5
-- This function will calculate the conjectures for a given list of mutators, number of mutants, properties, and function under test.
calculateConjectures :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [Conjecture]
calculateConjectures mutators nMutants props fut = do
  -- Execute the mutation function to get the raw results
  mutationResults <- executeMutation mutators nMutants props fut

  -- Determine all the PropertyAnalysis for each subset of properties
  let analyses = computeAnalyses $ transposeRawResults mutationResults

  -- Pass the analyses to computeConjectures to get the conjectures
  let conjectures = computeConjectures analyses

  -- Return the conjectures
  return conjectures

main :: IO ()
main = do
  let fut = multiplicationTable
  let properties = multiplicationTableProps
  let mutator = negateElements
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements] -- , anyList, emptyList]

  -- Determine the conjectures
  conjectures <- calculateConjectures mutators nMutants properties fut

  -- Print the conjectures
  putStrLn "The conjectures are: "
  forM_ conjectures print

-- The result of running this program is:
-- [1] → [4]
-- [2] → [0]
-- [2] → [4]
-- [0,1,3] → [2]
-- [2,3] → [1]

-- Time spent: 8 hours
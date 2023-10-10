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

computeConjectures :: [PropertyAnalysis] -> [Conjecture]
computeConjectures analyses = filterNonRedundant analysisSet analyses $ nub $ concatMap (conjecturesFor analysisSet) analyses
  where
    analysisSet = Set.fromList analyses

conjecturesFor :: Set.Set PropertyAnalysis -> PropertyAnalysis -> [Conjecture]
conjecturesFor set analysis = concatMap (compareAnalyses analysis) (Set.toList set)

compareAnalyses :: PropertyAnalysis -> PropertyAnalysis -> [Conjecture]
compareAnalyses a1 a2 = catMaybes [eqConjecture, imConjecture]
  where
    eqConjecture
      | mutantsKilled a1 == mutantsKilled a2 && mutantsSurvived a1 == mutantsSurvived a2 =
          Just $ Conjecture True False (Set.toList $ propertyIndices a1) (Set.toList $ propertyIndices a2) scoreValue nKilled nSurvivors
      | otherwise = Nothing
    imConjecture
      | Set.isSubsetOf (mutantsSurvived a1) (mutantsSurvived a2) =
          Just $ Conjecture False True (Set.toList $ propertyIndices a1) (Set.toList $ propertyIndices a2) scoreValue nKilled nSurvivors
      | otherwise = Nothing

    scoreValue = (nKilled * 100) `div` (nKilled + nSurvivors)
    nKilled = Set.size (Set.union (mutantsKilled a1) (mutantsKilled a2)) - Set.size (Set.intersection (mutantsKilled a1) (mutantsKilled a2))
    nSurvivors = Set.size (Set.intersection (mutantsSurvived a1) (mutantsSurvived a2))

filterNonRedundant :: Set.Set PropertyAnalysis -> [PropertyAnalysis] -> [Conjecture] -> [Conjecture]
filterNonRedundant analysisSet analyses = filter (\c -> not (isRedundant c allConjectures))
  where
    allConjectures = nub $ concatMap (conjecturesFor analysisSet) analyses -- Recompute all conjectures
    isRedundant :: Conjecture -> [Conjecture] -> Bool
    isRedundant conjecture = any (\other -> implies other conjecture && other /= conjecture)

    implies :: Conjecture -> Conjecture -> Bool
    implies c1 c2 = (cleft c1 `isSubsetOf` cleft c2) && (cright c1 `isSubsetOf` cright c2)

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf xs ys = all (`elem` ys) xs

-- Exercise 5
-- Implement function(s) that calculate the conjectures: properties that are equivalent, whose cases are subsets of other properties, etc.
-- calculateConjectures :: [Integer -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([a -> Integer -> Bool], [a -> Integer -> Bool])]
calculateConjectures mutators nMutants props fut = do
  mutationResults <- executeMutation mutators nMutants props fut
  let analyses = computeAnalyses $ transposeRawResults mutationResults

  let totalAnalysis = head $ filter (\analysis -> propertyIndices analysis == Set.fromList [0 .. length props - 1]) analyses

  let conjectures = computeConjectures analyses

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
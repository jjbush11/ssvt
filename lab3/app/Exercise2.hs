module Exercise2 where

import Control.Monad
import Exercise1
import FitSpec (testFitSpec)
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Data.List (transpose, subsequences)
import qualified Data.Set as Set

-- Define some type aliases
type PropertyId = Int
type MutantId = Int

-- A record to store the results of the analysis of a subset of properties.
data PropertyAnalysis = PropertyAnalysis
  { propertyIndices :: Set.Set PropertyId,
    totalMutants :: Int,
    nSurvivors :: Int,
    nKilled :: Int,
    score :: Int,  -- The score is nKilled / totalMutants * 100
    mutantsSurvived :: Set.Set MutantId,
    mutantsKilled :: Set.Set MutantId
  } deriving (Show, Eq, Ord)

-- The following function generates a list of mutation results, given a list of mutators, a number of mutants, a list of properties, and a function under test.
-- The result is then concatinated into a single list of [Bool].
-- This function was created after refactoring, so that we can use the results to compute the analysis for all possible subsets.
executeMutation :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[Bool]]
executeMutation mutators nMutants properties fut = do
  -- Generate a list of random inputs for the mutants
  inputs <- generate $ vectorOf (fromIntegral nMutants) (arbitrary `suchThat` (> 0))

  -- For each mutator, generate mutants using the generated inputs and test against properties
  results <- forM mutators $ \mutator -> do
    forM inputs $ \input -> do
      generate (mutate' mutator properties fut input)

  -- Filter out empty lists from each mutator, because they are not valid mutants
  let filteredResults = concatMap (filter (not . null)) results

  return filteredResults

-- This function converts a list of mutation results for a single property to a tuple of sets of surviving and killed mutants.
boolToSets :: [Bool] -> (Set.Set MutantId, Set.Set MutantId)
boolToSets booleans = go booleans 0 Set.empty Set.empty
  where
    go :: [Bool] -> MutantId -> Set.Set MutantId -> Set.Set MutantId -> (Set.Set MutantId, Set.Set MutantId)
    go [] _ survived killed = (survived, killed)
    go (b:bs) idx survived killed
      | b         = go bs (idx + 1) (Set.insert idx survived) killed
      | otherwise = go bs (idx + 1) survived (Set.insert idx killed)

-- Transpose the raw results to get a list of tuples of the form (propertyIndex, [Bool])
transposeRawResults :: [[Bool]] -> [(PropertyId, [Bool])]
transposeRawResults = zip [0..] . transpose

-- Helper function to compute a PropertyAnalysis record for a given subset of properties.
computeAnalysis :: [(Int, [Bool])] -> PropertyAnalysis
computeAnalysis properties =
    let totalMutantsValue = length $ snd $ head properties  -- Assumes all lists have the same length
    in PropertyAnalysis
      { propertyIndices = Set.fromList $ map fst properties,
        totalMutants = totalMutantsValue,
        nSurvivors = Set.size mutantsSurvived,
        nKilled = Set.size mutantsKilled,
        score = (Set.size mutantsKilled * 100) `div` totalMutantsValue,
        mutantsSurvived = mutantsSurvived,
        mutantsKilled = mutantsKilled
      }
  where
    -- Special case: use the first property to initialize the accumulators. Otherwise the intersection will always be empty.
    (initialSurvived, initialKilled) = boolToSets (snd (head properties))

    -- Compute sets of surviving and killed mutants by aggregating results of boolToSets over all properties in the subset.
    (mutantsSurvived, mutantsKilled) = foldl aggregate (initialSurvived, initialKilled) (tail properties)

    -- Helper function to aggregate sets of surviving and killed mutants.
    aggregate :: (Set.Set MutantId, Set.Set MutantId) -> (Int, [Bool]) -> (Set.Set MutantId, Set.Set MutantId)
    aggregate (survivedAcc, killedAcc) (_, bools) =
      let (survived, killed) = boolToSets bools
      in (Set.intersection survivedAcc survived, Set.union killedAcc killed)

-- Exercise 2
-- Where the first argument is the mutators,
-- the second argument is the number of mutants (4000 in the FitSpec example),
-- the third argument is the list of properties, and
-- the fourth argument is the function under test (the multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
countSurvivors :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Integer
countSurvivors mutators nMutants properties fut = do
  -- We first execute the executeMutation function to get the raw results
  mutationResults <- executeMutation mutators nMutants properties fut

  -- We then apply the computeAnalysis function to get a PropertyAnalysis record. 
  let transposedResults = transposeRawResults mutationResults

  let analysis = computeAnalysis transposedResults

  return $ fromIntegral $ nSurvivors analysis

-- Document the effect of which mutations are used and which properties are used on the number of survivors.
main :: IO ()
main = do
  -- Arguments passed to the countSurvivors function
  let fut = multiplicationTable
  let properties = multiplicationTableProps
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements] --, anyList, emptyList]

  -- Determine the number of survivors
  survivors <- countSurvivors mutators nMutants properties fut
  putStrLn $ "The number of survivors is: " ++ show survivors

  -- Based on the properties used, we can see that the number of survivors is 0.
  -- This means that the combination of all properties is able to kill all mutants. 
  -- In the context of mutation testing, this means that the set of properties is complete.

  -- Since the number of survivors is 0, we can also conclude that a set of less mutators will have 0 survivors.
  let lessMutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, powElements]
  survivorsLessMutators <- countSurvivors lessMutators nMutants properties fut
  putStrLn $ "The number of survivors with less mutators: " ++ show survivorsLessMutators

  -- We can also conclude that a set of less properties will have more survivors.
  -- Removing property prop_linear from the list of properties will result in 397 survivor.
  let lessProperties = [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_moduloIsZero]
  survivorsLessProperties <- countSurvivors mutators nMutants lessProperties fut
  putStrLn $ "The number of survivors with less properties: " ++ show survivorsLessProperties


-- Time spent: 8 hours
module Exercise2 where

import Control.Monad
import Exercise1
import FitSpec (testFitSpec)
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Data.List (transpose, subsequences)
import qualified Data.Set as Set

-- input can be constant, e.g. 1

type PropertyId = Int
type MutantId = Int

-- A record to store the results of the analysis of a subset of properties.
data PropertyAnalysis = PropertyAnalysis
  { propertyIndices :: Set.Set PropertyId,
    rawResult :: [Bool],
    totalMutants :: Int,
    nSurvivors :: Int,
    nKilled :: Int,
    score :: Int,  -- The score is nKilled / totalMutants * 100
    mutantsSurvived :: Set.Set MutantId,
    mutantsKilled :: Set.Set MutantId
  } deriving (Show)

executeMutation :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[Bool]]
executeMutation mutators nMutants properties fut = do
  -- Generate a list of random inputs for the mutants
  -- The same inputs are used for each mutator
  inputs <- generate $ vectorOf (fromIntegral nMutants) (arbitrary `suchThat` (> 0))

  -- For each mutator, generate mutants using the generated inputs and test against properties
  results <- forM mutators $ \mutator -> do
    forM inputs $ \input -> do
      generate (mutate' mutator properties fut input)

  -- Filter out empty lists from each mutator, because they are not valid mutants
  let filteredResults = concatMap (filter (not . null)) results

  return filteredResults
  where
    propertiesIndices = [1 .. (length properties)]

-- This function needs to give an index to each boolean, then depending on if the boolean is true (survived) or not (killed)
-- Add this index to the first set or the second. Return a tuple of these sets. 
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
    let totalMutantsValue = length . head . map snd $ properties  -- Assumes all lists have the same length
    in PropertyAnalysis
      { propertyIndices = Set.fromList $ map fst properties,
        rawResult = map snd properties >>= id,  -- Flatten the list of Bool lists
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
  res <- executeMutation mutators nMutants properties fut
  -- We first apply and to each list of result of the mutate' function, which is a list of Bool.
  -- If it then is True, it means that all properties are satisfied, and the mutant has survived.
  -- We then filter out the False values, because we are interested in the number of survivors.
  -- After that, we count the number of survivors by determining the length of the list.
  -- return $ fromIntegral $ length $ filter id $ map and res

  -- print res

  -- We then apply the computeAnalysis function to get a PropertyAnalysis record. 
  let transposedResults = transposeRawResults res
  let raw = map (length . snd) transposedResults
  let indices = map fst $ transposeRawResults res

  print raw
  print indices
  -- print if empty lists exist in the raw results
  print $ elem [] res
  -- print transposedResults
  print "This worked..."

  let analysis = computeAnalysis transposedResults

  -- print survivors
  print $ nSurvivors analysis

  return $ fromIntegral $ length $ filter id $ map and res


printIO :: (Show a) => IO a -> IO ()
printIO = (>>= print)

-- Document the effect of which mutations are used and which properties are used on the number of survivors.
allElementsEqual :: (Eq a) => [a] -> Bool
allElementsEqual [] = True
allElementsEqual (x : xs) = all (== x) xs

main :: IO ()
main = do
  -- Experimenting with the mutate' function
  let fut = multiplicationTable
  let properties = multiplicationTableProps
  let mutator = negateElements
  input <- generate arbitrary :: IO Integer
  results <- generate (mutate' mutator properties fut input)
  print results -- This will print a list of Bool values
  let nMutants = 4000
  let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements] --, anyList, emptyList]

  -- Experimenting with the executeMutation function
  res <- executeMutation mutators nMutants properties fut

  -- Experimenting with the countSurvivors function
  survivors <- countSurvivors mutators nMutants properties fut
  print survivors

--   printIO $ generate (countSurvivors' mutators nMutants properties fut)

-- Run fitspec for comparison to our own implementation
-- testFitSpec
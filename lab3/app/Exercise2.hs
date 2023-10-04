module Exercise2 where

import Test.QuickCheck
import Control.Monad
import Mutation
import MultiplicationTable
import Exercise1

-- Exercise 2
-- Where the first argument is the number of mutants (4000 in the FitSpec example), 
-- the second argument is the list of properties, and 
-- the third argument is the function under test (the multiplication table function in this case).
-- A fourth argument is added to accept a list of mutators.
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> Integer
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Integer
countSurvivors nMutants properties fut mutators = do
    -- Generate a list of random inputs for the mutants
    inputs <- generate $ vectorOf (fromIntegral nMutants) arbitrary

    -- For each input, generate mutants using each mutator and test against properties
    results <- forM inputs $ \input -> do
        mutantsResults <- forM mutators $ \mutator -> do
            result <- generate (mutate' mutator properties fut input)
            return (not $ or result)  -- True if the mutant survived all properties
        return (or mutantsResults)  -- True if the mutant survived any mutator

    -- Count the number of True values (survivors)
    return $ fromIntegral $ length $ filter id results

-- Document the effect of which mutations are used and which properties are used on the number of survivors.


main :: IO ()
main = do
    let fut = multiplicationTable
    let properties = multiplicationTableProps
    let mutator = negateElements
    input <- generate arbitrary :: IO Integer
    results <- generate (mutate' mutator properties fut input)
    print results  -- This will print a list of Bool values

    let nMutants = 4000
    let mutators = [sortList, duplicateElements]
    survivors <- countSurvivors nMutants properties fut mutators
    print survivors

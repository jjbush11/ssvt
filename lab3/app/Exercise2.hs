module Exercise2 where

import Test.QuickCheck
import Control.Monad
import Mutation
import MultiplicationTable
import Exercise1
import FitSpec (testFitSpec)

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
    -- The same inputs are used for each mutator
    -- By doing this, we can compare the effectiveness of the mutators
    inputs <- generate $ vectorOf (fromIntegral nMutants) arbitrary

    -- For each mutator, generate mutants using the generated inputs and test against properties
    results <- forM mutators $ \mutator -> do
        forM inputs $ \input -> do
            result <- generate (mutate' mutator properties fut input)
            -- -- If one of the properties fails, return False
            -- -- If the result is empty, return False as well 
            -- return $ any (== False) result
            -- If one of the properties fails, return False
            -- If the result is empty, return False as well 
            return $ and result

    return $ fromIntegral $ length $ filter id $ concat results

countSurvivors' :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]] -> IO Integer
countSurvivors' nMutants properties fut mutators = do
    -- Generate a list of random inputs for the mutants
    -- The same inputs are used for each mutator
    -- By doing this, we can compare the effectiveness of the mutators
    inputs <- generate $ vectorOf (fromIntegral nMutants) arbitrary

    -- For each mutator, generate mutants using the generated inputs and test against properties
    results <- forM mutators $ \mutator -> do
        forM inputs $ \input -> do
            result <- generate (mutate' mutator properties fut input)
            -- -- If one of the properties fails, return False
            -- -- If the result is empty, return False as well 
            -- return $ any (== False) result
            -- If one of the properties fails, return False
            -- If the result is empty, return False as well 
            return $ and result

    return $ fromIntegral $ length $ filter id $ concat results

-- Document the effect of which mutations are used and which properties are used on the number of survivors.

main :: IO ()
main = do
    -- Experimenting with the mutate' function
    let fut = multiplicationTable
    let properties = multiplicationTableProps
    let mutator = negateElements
    input <- generate arbitrary :: IO Integer
    results <- generate (mutate' mutator properties fut input)
    print results  -- This will print a list of Bool values

    let nMutants = 4000
    let mutators = [sortList, duplicateElements, singleElementList, negateElements]
    survivors <- countSurvivors nMutants properties fut mutators
    print survivors

    -- Run fitspec for comparison to our own implementation
    -- testFitSpec 

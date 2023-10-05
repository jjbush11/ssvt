module Exercise2 where

import Test.QuickCheck
import Control.Monad
import Mutation
import MultiplicationTable
import Exercise1
import FitSpec (testFitSpec)

-- input can be constant, e.g. 1 



-- Exercise 2
-- Where the first argument is the number of mutants (4000 in the FitSpec example), 
-- the second argument is the list of properties, and 
-- the third argument is the function under test (the multiplication table function in this case).
-- A fourth argument is added to accept a list of mutators.
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> Integer
countSurvivors :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Integer
countSurvivors mutators nMutants properties fut = do
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
            -- If all of the properties hold (i.e. all values True), return True (survived)
            -- If the result is empty, return False as well 
            return $ and result

    return $ fromIntegral $ length $ filter id $ concat results

-- executeMutation :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO [[Bool]]
-- executeMutation :: (Integral a1, Eq a2) => [a2 -> Gen a2] -> a1 -> [a2 -> Integer -> Bool] -> (Integer -> a2) -> IO [[[Bool]]]
executeMutation mutators nMutants properties fut = do
    -- Generate a list of random inputs for the mutants
    -- The same inputs are used for each mutator
    -- By doing this, we can compare the effectiveness of the mutators
    inputs <- generate $ vectorOf (fromIntegral nMutants) (arbitrary `suchThat` (> 0))

    -- For each mutator, generate mutants using the generated inputs and test against properties
    results <- forM mutators $ \mutator -> do
        forM inputs $ \input -> do
            result <- generate (mutate' mutator properties fut input)
            return result

    -- filter out empty lists from each mutator
    return $ map (filter (not . null)) results

printIO :: Show a => IO a -> IO ()
printIO = (>>= print)

-- Document the effect of which mutations are used and which properties are used on the number of survivors.
allElementsEqual :: Eq a => [a] -> Bool
allElementsEqual [] = True
allElementsEqual (x:xs) = all (== x) xs


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
    let mutators = [sortList, duplicateElements, singleElementList, negateElements, removeElements, zeroElements, shuffleElements, powElements, multiplyByArbitrary, addOneToElements]
    survivors <- countSurvivors mutators nMutants properties fut
    print survivors

    -- Run fitspec for comparison to our own implementation
    -- testFitSpec 

    -- Experimenting with the executeMutation function
    res <- executeMutation mutators nMutants properties fut

    -- Are all elements within the list the same? 
    let iets = res !! 3
    print $ allElementsEqual iets
    print $ map head (tail res)
    print $ map allElementsEqual (tail res)
    -- print res
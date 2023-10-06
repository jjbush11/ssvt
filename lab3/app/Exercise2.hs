module Exercise2 where

import Test.QuickCheck
import Control.Monad
import Mutation
import MultiplicationTable
import Exercise1
import FitSpec (testFitSpec)

-- input can be constant, e.g. 1 

data Result = Result {
    rawResult :: [Bool],
    survivors :: Int,
    nMutants :: Int,
    propertyIndices :: [Int]
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
    return $ concatMap (filter (not . null)) results
    where
        propertiesIndices = [1..(length properties)]

executeMutation' :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen [[Bool]]
executeMutation' mutators nMutants properties fut = do
    -- Generate a list of random inputs for the mutants
    -- The same inputs are used for each mutator
    let input = 1

    -- For each mutator, create n generators using the generated inputs and test against properties
    results <- forM mutators $ \mutator -> do
        replicateM (fromIntegral nMutants) $ do
            mutate' mutator properties fut input

    -- Filter out empty lists from each mutator, because they are not valid mutants
    return $ concatMap (filter (not . null)) results
    where
        propertiesIndices = [1..(length properties)]

-- Exercise 2
-- Where the first argument is the mutators,
-- the second argument is the number of mutants (4000 in the FitSpec example), 
-- the third argument is the list of properties, and 
-- the fourth argument is the function under test (the multiplication table function in this case).
-- The output is the number of surviving mutants (0 in the FitSpec example).
-- countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> [[Integer] -> Gen [Integer]] -> (Integer -> [Integer]) -> Integer
countSurvivors :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> IO Integer
countSurvivors mutators nMutants properties fut = do
    -- 
    res <- executeMutation mutators nMutants properties fut
    -- We first apply and to each list of Bool values, 
    -- If it then is True, it means that all properties are satisfied, and the mutant has survived.
    -- After that, we count the number of survivors by counting the number of True values.
    return $ fromIntegral $ length $ filter id $ map and res

countSurvivors' :: [[Integer] -> Gen [Integer]] -> Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> Gen Integer
countSurvivors' mutators nMutants properties fut = do
    -- 
    res <- executeMutation' mutators nMutants properties fut
    -- We first apply and to each list of Bool values, 
    -- If it then is True, it means that all properties are satisfied, and the mutant has survived.
    -- After that, we count the number of survivors by counting the number of True values.
    return $ fromIntegral $ length $ filter id $ map and res

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

    -- Experimenting with the executeMutation function
    res <- executeMutation mutators nMutants properties fut

    -- Experimenting with the executeMutation' function
    res' <- generate (executeMutation' mutators nMutants properties fut)

    putStrLn $ "Length of two functions, are equal " ++ show (length res == length res')
    putStrLn $ "The lengths are " ++ show (length res) ++ " and " ++ show (length res')

    -- Experimenting with the countSurvivors function
    survivors <- countSurvivors mutators nMutants properties fut
    print survivors

    -- Experimenting with the countSurvivors' function
    printIO $ generate (countSurvivors' mutators nMutants properties fut)

    -- Run fitspec for comparison to our own implementation
    -- testFitSpec 
module Mutation where

import Data.List
import Data.Maybe
import Debug.Trace
import MultiplicationTable
import Test.QuickCheck

-- Applies a mutator to a property and function under test, then returns whether the mutant is killed (Just False), whether it lives (Just True), or that the mutant did not change the output (Nothing)
mutate :: (Eq a) => (a -> Gen a) -> (a -> Integer -> Bool) -> (Integer -> a) -> Integer -> Gen (Maybe Bool)
mutate mutator prop fut input = mutation >>= \mutant -> mutateOrNothing output mutant (Just <$> propertyExecutor prop mutant input)
  where
    output = fut input
    mutation = mutator output

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing :: (Eq a) => a -> a -> Gen (Maybe Bool) -> Gen (Maybe Bool)
mutateOrNothing output mutant res
  | output == mutant = return Nothing
  | otherwise = res

propertyExecutor :: (Eq a) => (a -> Integer -> Bool) -> a -> Integer -> Gen Bool
propertyExecutor prop mutant x = return $ prop mutant x

-- | Applies a mutator to a property and function under test, then returns whether the mutant is killed (False), whether it lives (True), or that the mutant did not change the output (empty list)
--
-- The function takes a mutator function, a list of property functions, a function under test, an input value for the function under test.
-- The mutator function is applied to the output of the function under test, and the resulting mutant is tested against each property function in the list.
-- The function returns a list of Boolean values, where each value represents whether the corresponding mutant was killed (False), whether it lived (True), or whether it did not change the output (empty list).
mutate' :: (Eq a) => (a -> Gen a) -> [a -> Integer -> Bool] -> (Integer -> a) -> Integer -> Gen [Bool]
mutate' mutator props fut input = mutation >>= \mutant -> mutateOrNothing' output mutant (propertyExecutor' props mutant input)
  where
    output = fut input -- e.g. [1,2,3,4,5,6,7,8,9,10]
    mutation = mutator output -- e.g. [1,2,3,4,5,6,7,8,9,10,11]

propertyExecutor' :: (Eq a) => [a -> Integer -> Bool] -> a -> Integer -> Gen [Bool]
propertyExecutor' prop mutant x = return $ map (\y -> y mutant x) prop

-- Returns the mutated result, or nothing if the result is identical to the original output
mutateOrNothing' :: (Eq a) => a -> a -> Gen [Bool] -> Gen [Bool]
mutateOrNothing' output mutant res
  | output == mutant = return []
  | otherwise = res

-- == Mutators ==
-- Adds elements to the beginning and/or end of an output list
addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  nums2 <- arbitrary :: Gen [Integer]
  return $ nums ++ xs ++ nums2

-- Removes 1 to (length - 1) elements from an output list
removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (1, length xs) >>= \x -> return $ take x xs

-- Any list
anyList :: [Integer] -> Gen [Integer]
anyList xs = arbitrary

mutators :: [[Integer] -> Gen [Integer]]
mutators = [anyList, removeElements, addElements]

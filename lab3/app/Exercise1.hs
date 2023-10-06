module Exercise1 where

import Test.QuickCheck
import Data.List

-- Exercise 1
-- We provide some mutators to mutate the output of the list in Mutation.hs. Write down which
-- types of output are not yet covered by these mutators, and about their weakness/strength.
-- Come up with a list of other mutators and implement (a subset of) them.

-- Potential mutators: 
-- 1. Sorting 
-- 2. Duplicate elements
-- 3. Empty list
-- 4. Single element list
-- 5. Negating elements
-- 6. Zeroing elements

-- Additional Mutators
-- Sorting mutator: Sorts the list
-- Strength: It can identify if the property is order dependent
-- Weakness: It may not be applicable in cases where the order of the list is not important
sortList :: [Integer] -> Gen [Integer]
sortList xs = return $ sort xs

-- Duplicate elements mutator: Duplicates the elements in the list
-- Strength: Is able to check if the function handles duplicate elements correctly
-- Weakness: Might not be relevant in cases where duplicate elements have no effect on the output
duplicateElements :: [Integer] -> Gen [Integer]
duplicateElements xs = return $ xs ++ xs

-- Empty List: Returns an empty list
-- Although removeElements mutator can return an empty list, it is not guaranteed to do so
-- It might therefore be useful to have a mutator that always returns an empty list
-- Strength: Can be used to check if the function handles empty lists correctly
-- Weakness: Specific to empty list scenarios, and might not be applicable in other cases
emptyList :: [Integer] -> Gen [Integer]
emptyList xs = return []

-- Single element list: Returns a list with a single element
-- Strength: Useful to verify if the function handles single element lists correctly
-- Weakness: Only specific to single element list scenarios
singleElementList :: [Integer] -> Gen [Integer]
singleElementList xs = return [head xs]

-- Negating elements: Negates all elements in the list
-- Strength: 
-- * Useful to check for issues regarding sign-dependent behaviour 
-- * Can check if the function handles negative numbers correctly
-- Weakness: Not very useful for sign agnostic functions
negateElements :: [Integer] -> Gen [Integer]
negateElements xs = return $ map negate xs

-- Zeroing elements: Sets all elements in the list to zero
-- Strength: Useful to check if the function handles zero correctly
-- Weakness: Not very useful for functions that do not handle zero
zeroElements :: [Integer] -> Gen [Integer]
zeroElements xs = return $ map (const 0) xs

-- Shuffling elements: Shuffles the elements in the list
-- Strength: Useful to check if the function handles shuffled lists correctly
-- Weakness: Not very useful for functions that do not handle shuffled lists
shuffleElements :: [Integer] -> Gen [Integer]
shuffleElements xs = do
  shuffle xs

powElements :: [Integer] -> Gen [Integer]
powElements xs = return $ map (^2) xs

-- Multiply by arbitrary: Multiplies all elements in the list by an arbitrary number
multiplyByArbitrary :: [Integer] -> Gen [Integer]
multiplyByArbitrary xs = do
  n <- arbitrary
  return $ map (* n) xs

addOneToElements :: [Integer] -> Gen [Integer]
addOneToElements xs = return $ map (+1) xs


main :: IO ()
main = do
  print "Hello World"
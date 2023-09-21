module Lab1.Exercise9 where

import Data.List
import Test.QuickCheck

-- Exercise 9
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys

-- Generators
-- Define a generator for even positive integers
positiveInt :: Gen Int
positiveInt = arbitrary

-- Define a generator for lists containing integers
intList :: Gen [Int]
intList = do
  n <- positiveInt
  let values = [1 .. n]
  shuffle values

-- Test props
-- This function checks if two equal lists are permutation of each other
prop_checkIfEqualListsAreAPermutationOfItself :: [Int] -> Bool
prop_checkIfEqualListsAreAPermutationOfItself list = isPermutation list list

-- This function checks if two empty lists are permutation of each other
prop_checkIfEmptyListIsAPermutationOfItself :: Bool
prop_checkIfEmptyListIsAPermutationOfItself = isPermutation ([] :: [Integer]) ([] :: [Integer])

replaceFirst :: a -> [a] -> [a]
replaceFirst _ [] = [] -- If the list is empty, return an empty list (no replacement)
replaceFirst newVal (x : xs) = newVal : xs

prop_checkIfUnequalListArePermutations :: [Int] -> Bool
prop_checkIfUnequalListArePermutations list = list `isPermutation` replaceFirst (-10000) list

prop_checkIfListWithDifferentLengthsArePermutations :: [Int] -> Bool
prop_checkIfListWithDifferentLengthsArePermutations list = list `isPermutation` (10 : list)

getFirstElement :: [a] -> a
getFirstElement [] = error "List is empty, cannot access the first element."
getFirstElement (x : _) = x

prop_checkIfShuffledListIsAPermutation :: [Int] -> Bool
prop_checkIfShuffledListIsAPermutation list = list `isPermutation` getFirstElement (permutations list)

main :: IO ()
main = do
  quickCheck $ forAll intList prop_checkIfEqualListsAreAPermutationOfItself
  quickCheck prop_checkIfEmptyListIsAPermutationOfItself
  quickCheck $ expectFailure $ forAll intList prop_checkIfUnequalListArePermutations
  quickCheck $ expectFailure $ forAll intList prop_checkIfListWithDifferentLengthsArePermutations
  quickCheck $ forAll intList prop_checkIfShuffledListIsAPermutation

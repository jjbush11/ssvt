module Exercise4 where

import Data.List
import Test.QuickCheck

-- Exercise 4
-- helper functions
myForAll :: [a] -> (a -> Bool) -> Bool
myForAll = flip all

-- Generators
-- Define a generator for even positive integers
positiveInt :: Gen Int
positiveInt = arbitrary

-- Generators
-- Define a generator for even positive integers
positiveIntSmallerThen20 :: Gen Int
positiveIntSmallerThen20 = arbitrary `suchThat` (< 9)

-- Define a generator for lists containing integers
intList :: Gen [Int]
intList = do
  n <- positiveInt
  let values = [1 .. n]
  shuffledValues <- shuffle values
  return shuffledValues

-- Define a generator for even positive integers
evenPositiveInt :: Gen Int
evenPositiveInt = arbitrary `suchThat` even

-- Define a generator for lists of even length containing integers
evenLengthIntList :: Gen [Int]
evenLengthIntList = do
  n <- evenPositiveInt
  let values = [1 .. n]
  shuffledValues <- shuffle values
  return shuffledValues

-- Define a generator for odd positive integers
oddPositiveInt :: Gen Int
oddPositiveInt = arbitrary `suchThat` odd

-- Define a generator for lists of odd length containing integers
oddLengthIntList :: Gen [Int]
oddLengthIntList = do
  n <- oddPositiveInt
  let values = [1 .. n]
  shuffledValues <- shuffle values
  return shuffledValues

-- Props
-- isDerangement tests
-- This function checks if two known derangements are derangements of each other
prop_checkKnownDerangements1 :: Bool
prop_checkKnownDerangements1 = isDerangement [1, 0] [0, 1]

-- This function checks if two known derangements are derangements of each other
prop_checkKnownDerangements2 :: Bool
prop_checkKnownDerangements2 = isDerangement [0, 1, 2] [1, 2, 0]

-- This function checks if two known derangements are derangements of each other
prop_checkKnownDerangements3 :: Bool
prop_checkKnownDerangements3 = isDerangement [0, 1, 2] [2, 0, 1]

-- This function checks if two known derangements are derangements of each other
prop_checkKnownDerangements4 :: Bool
prop_checkKnownDerangements4 = isDerangement [1, 2, 1, 2, 1] [2, 1, 2, 1, 2]

-- This function checks if two known derangements are derangements of each other
prop_checkKnownDerangements5 :: Bool
prop_checkKnownDerangements5 = isDerangement [0, 1, 2, 3, 4, 5] [2, 0, 4, 5, 1, 3]

-- This function checks if the a list and the reverse of a list are derangements of each other
-- This should be the case if the list is even ([0,1] is a derangement of [1,0])
-- This shouldn't be the case if the list is uneven ([0,1,2] is not a derangement of [2,1,0])
prop_reverseOfListIsDerangement :: [Int] -> Bool
prop_reverseOfListIsDerangement list = isDerangement list (reverse list)

-- This function checks if two equal lists are deragements of each other
prop_checkIfEqualListsAreNotADerangementOfItself :: [Int] -> Bool
prop_checkIfEqualListsAreNotADerangementOfItself list = isDerangement list list

-- This function checks if two empty lists are deragements of each other
prop_checkIfEmptyListIsADerangementOfItself :: Bool
prop_checkIfEmptyListIsADerangementOfItself = isDerangement ([] :: [Integer]) ([] :: [Integer])

-- This function checks if duplicate list with one element are deragements of each other
prop_checkIfAListWithOneElementIsADerangementOfItself :: Int -> Bool
prop_checkIfAListWithOneElementIsADerangementOfItself n = isDerangement [n] [n]

-- This function checks if two list with unequal lengths are deragements of each other
prop_checkIfUnequalListsCanBeADerangement :: Bool
prop_checkIfUnequalListsCanBeADerangement = isDerangement [1, 0] [0, 1, 2]

-- deran test
-- This function checks if all the generated derangements have the same length as the orinal array
prop_checkIfTheLenghtOfDerangementsAreTheSameAsTheOriginal :: Int -> Bool
prop_checkIfTheLenghtOfDerangementsAreTheSameAsTheOriginal n = myForAll (deran n) (\x -> length x == length [0 .. n - 1])

-- This function checks if all the generated derangements are indeed derangements of the original
prop_checkIfEveryElementIsADerangementOfTheOriginal :: Int -> Bool
prop_checkIfEveryElementIsADerangementOfTheOriginal n = myForAll (deran n) (isDerangement [0 .. n - 1])

-- This function checks if two lists are derangements of each other.
-- The functions loops through the lists recursively.
-- It constanstly compares the heads of the list and then parses the tail of the list to itself.
-- If the tails of the list are equal to each other the list is not a dergement.
-- And if the lenght of the list are different then it also isn't a derangement.
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement original derangement
  | length derangement /= length original = False
  | null derangement = True
  | otherwise = (head original /= head derangement) && isDerangement (tail original) (tail derangement)

-- This function generates all the derangement of a list.
-- The algorithm first calculates all the permutations of the orignal list.
-- After calculating all the permutations it filters out all the lists
-- that aren't derangements of the orignal list.
-- The list you end up with only contains derangement of the original list.
deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x [0 .. n - 1]) (permutations [0 .. n - 1])

main :: IO ()
main = do
  -- isDerangement tests

  -- We test if the isDerangement function returns true for a known derangement.
  -- We test this with 5 different 'known derangements'.
  -- The test passes.
  -- This means that the isDerangement function works for a small sample set of Lists.
  quickCheck prop_checkKnownDerangements1
  quickCheck prop_checkKnownDerangements2
  quickCheck prop_checkKnownDerangements3
  quickCheck prop_checkKnownDerangements4
  quickCheck prop_checkKnownDerangements5

  -- We test if the reverse of a even list of integers is a derangement of the original list.
  -- This should be the case because ([0,1] is a derangement of [1,0])
  -- The test passes.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck (forAll evenLengthIntList prop_reverseOfListIsDerangement)

  -- We test if the reverse of a uneven list of integers is a derangement of the original list.
  -- This shouldn't be the case because ([0,1,2] is not a derangement of [2,1,0])
  -- The test passes fails as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ expectFailure $ forAll oddLengthIntList prop_reverseOfListIsDerangement

  -- We test if the origal list is a derangement of the same list.
  -- This shouldn't be the case because ([1,0]) for example is not a derangement of ([1,0])
  -- The test passes fails as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ expectFailure $ forAll intList prop_checkIfEqualListsAreNotADerangementOfItself

  -- We test if a empty list is a derangement of itself.
  -- This should be the case and the test passes as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck prop_checkIfEmptyListIsADerangementOfItself

  -- We test if a list with only one element is a derangement of itself.
  -- This shouldn't be the case and the test fails as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ expectFailure $ forAll positiveInt prop_checkIfAListWithOneElementIsADerangementOfItself

  -- We test if a two lists who have a different different lenghts can be derangements of eacht other.
  -- This shouldn't be the case and the test fails as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ expectFailure prop_checkIfUnequalListsCanBeADerangement

  -- deran tests

  -- We test if all the generated derangements have the same lengths (which should be the case)
  -- This should be the case and the test passes as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ forAll positiveIntSmallerThen20 prop_checkIfTheLenghtOfDerangementsAreTheSameAsTheOriginal

  -- We test if al the generated derangements by the deran functions
  -- are indeed derangements when compared to the orignal list.
  -- This should be the case and the test passes as expected.
  -- This suggest that the functions works as expected in this particular case.
  quickCheck $ forAll positiveIntSmallerThen20 prop_checkIfEveryElementIsADerangementOfTheOriginal

-- Time spent: 3 hours
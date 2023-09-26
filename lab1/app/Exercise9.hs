module Exercise9 where

import Data.List
import Test.QuickCheck

-- Exercise 9
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` permutations ys

-- Generators
-- Define a generator for even positive Ints
positiveInt :: Gen Int
positiveInt = arbitrary `suchThat` (> 1)

-- Define a generator for lists containing Ints
intList :: Gen [Int]
intList = do
  n <- positiveInt
  let values = [1 .. n]
  shuffle values

-- Define a generator for lists containing Ints with a list size
intListWithSize :: Int -> Gen [Int]
intListWithSize n = do
  let values = [1 .. n]
  shuffle values

-- Test props

-- This checks if the isPermutation function is symmetric
-- If xs is a permutation of ys -> ys should also be a permutation of xs
prop_symmetry :: [Int] -> [Int] -> Bool
prop_symmetry xs ys = isPermutation xs ys == isPermutation ys xs

-- Lists should be of equal lenght to be permutations of eachother.
prop_equalLength :: [Int] -> [Int] -> Bool
prop_equalLength xs ys = isPermutation xs ys == (length xs == length ys)

-- This checks if two equal lists are permutation of each other
prop_reflexivity :: [Int] -> Bool
prop_reflexivity xs = isPermutation xs xs

-- This checks if two empty lists are permutation of each other
prop_checkIfEmptyListIsAPermutationOfItself :: Bool
prop_checkIfEmptyListIsAPermutationOfItself = isPermutation ([] :: [Int]) ([] :: [Int])

replaceFirst :: a -> [a] -> [a]
replaceFirst _ [] = [] -- If the list is empty, return an empty list (no replacement)
replaceFirst newVal (x : xs) = newVal : xs

-- This checks if two unequal lists are permutation of each other
prop_checkIfUnequalListArePermutations :: [Int] -> Bool
prop_checkIfUnequalListArePermutations list = list `isPermutation` replaceFirst (-10000) list

-- This checks if two lists with different lengths are permutation of each other
prop_checkIfListWithDifferentLengthsArePermutations :: [Int] -> Bool
prop_checkIfListWithDifferentLengthsArePermutations list = list `isPermutation` (10 : list)

-- Manually choosen lists
a :: [Int]
a = [1, 2, 3]

b :: [Int]
b = [3, 2, 1]

c :: [Int]
c = [4, 5, 6]

d :: [Int]
d = [1, 2, 3, 4]

e :: [Int]
e = []

f :: [Int]
f = [2, 2, 2]

g :: [Int]
g = [1, 2, 3, 3]

h :: [Int]
h = [-1, -2, -3]

i :: [String]
i = ["kaasje", "koekje", "drankje"]

j :: [String]
j = ["kaakie", "koekie", "drankie"]

k :: [String]
k = ["koekje", "drankje", "kaasje"]

listOfPredefinedLists :: [[Int]]
listOfPredefinedLists = [a, b, c, d, e, f, g, h]

wellChoosenListsCheck :: Bool
wellChoosenListsCheck =
  isPermutation a a
    && isPermutation a b -- Equal Lists are permutations of eachother
    && not (isPermutation a c) -- Lists with the same elements in a different order are permutations of eachother
    && not (isPermutation a d) -- Lists with different elements are not permutations of eachother
    && not (isPermutation e a) -- A shorter list is not a permutation of a longer list
    && not (isPermutation a f) -- An empty list is not a permutation of list with items
    && not (isPermutation a g) -- A lists with different items is not a permutation of a list with all the same itesm
    && not (isPermutation a h) -- A list with duplicate items is not a permutation of a list without duplicate items
    && isPermutation h h -- A list is not a permutation of a list with negative items
    && isPermutation i i -- Negative numbers are also allowed
    && isPermutation f f -- Works for strings
    && isPermutation e e -- A list with all the same items is a permutation of itself
    && not (isPermutation d g) -- Empty lists are permutations of eachotehr
    && not (isPermutation d a) -- Equel lenght lists with 1 different item are not permutations of eachother
    && isPermutation i k -- A longer list is not a permutation of a shorter list
    && not (isPermutation i j) -- Works for strings
    -- Works for strings

main :: IO ()
main = do
  quickCheck prop_checkIfEmptyListIsAPermutationOfItself
  quickCheck $ expectFailure $ forAll intList prop_checkIfUnequalListArePermutations
  quickCheck $ expectFailure $ forAll intList prop_checkIfListWithDifferentLengthsArePermutations
  quickCheck wellChoosenListsCheck
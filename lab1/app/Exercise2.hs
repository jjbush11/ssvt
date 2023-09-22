module Exercise2 where

import Data.List
import Test.QuickCheck

-- Exercise 2

-- A function to check if all elements in a list are unique.
-- It makes use of nub, which removes duplicates from a list.
-- If there are no duplicate elements,
-- the length of the list and the list with all duplicates removed should be equal.
unique :: Eq a => [a] -> Bool
unique xs = length xs == length (nub xs)

-- In a list of subsequences, there should be no duplicate subsequences.
-- Thus we test that all subsequences are unique with above mentioned function.
prop_uniqueness :: Eq a => [a] -> Bool
prop_uniqueness xs = unique $ subsequences xs

-- Identity property
-- The list of subsequences should contain the original sequence.
prop_identity :: Eq a => [a] -> Bool
prop_identity xs = xs `elem` subsequences xs

-- Containment property
-- Check if all subsequences contained are indeed subsequences of the original sequence.
prop_containment :: Eq a => [a] -> Bool
prop_containment xs = all (`isSubsequenceOf` xs) (subsequences xs)

-- ​​The power set of an empty set has only one element.
prop_baseCase :: Bool
prop_baseCase = 1 == length (subsequences [])

-- A mathematical property of subsequences is that if |A| = n, then |P(A)| = 2^n
-- If the subsequences function behaves correcly, the length of it applied to a sequence,
-- should be the same as two to the power of the length of the original sequence.
prop_lengthOfPowerSet :: [a] -> Bool
prop_lengthOfPowerSet xs = 2 ^ length xs == length (subsequences xs)

prop_test :: [a] -> [a] -> Bool
prop_test xs ys = length (subsequences (xs ++ ys)) == 2 ^ (length xs + length ys)

-- A QuickCheck generator which generates lists of a maximum length of 100
-- A maximum length was chosen, because otherwise the testing of the properties
-- would take a big amount of resources and wouldn't finish timely.
-- We started out at 100, but this took to long and we scaled down to 20.
listGeneratorWithMaxLength :: Gen [Int]
listGeneratorWithMaxLength = sized $ \n -> do
  k <- choose (0, 20)
  vectorOf k arbitrary

main :: IO ()
main = do
  -- We tried to test on unique results of the subsequenses function.
  -- However, the subsequence will not return a unique list of subsequences.
  -- For example, if we apply subsequences on the string "\ETX\ETX".
  -- This will return the following subsequences: ["","\ETX","\ETX","\ETX\ETX"]
  -- Which contains "\ETX" two times, meaning the outcome is not unique.
  -- This is appearently the intended behaviour of the subsequences function.
  -- Thus we expect the following test to fail.
  quickCheck $ prop_uniqueness "\ETX\ETX"

  -- We test the result of a empty list.
  -- The test passes.
  -- This means that the empty list is a subsequence of the empty list.
  quickCheck prop_baseCase

  -- We test the identity property on a list of integers.
  -- The test passes for 100 tests.
  -- This means that the list of subsequences contains the original sequence and thus the identity property holds.
  quickCheck $ forAll listGeneratorWithMaxLength prop_identity

  -- We test the containment property on a list of integers.
  -- The test passes for 100 tests.
  -- This means that all subsequences contained are indeed subsequences of the original sequence.
  -- Thus the containment property holds.
  quickCheck $ forAll listGeneratorWithMaxLength prop_containment

  -- We believe the prop_identity and prop_containment properties are sufficient to test the correctness of the subsequences function.

  -- We are testing a mathematical fact, namely that if A is a finite set with |A| = n, then
  -- P(A) = 2^n. However, we are not proving this by these tests. A mathematical proof by induction should be done for that instead.
  quickCheck $ forAll listGeneratorWithMaxLength prop_lengthOfPowerSet

-- It was quite hard to test above mentioned properties, this was due to a
-- lack of understanding of the subsequences function and its specification.
-- Furthermore, it was hard due to excessive resource usage. Because of that we had to scale down on the size of the lists we were testing on.

-- Time spent: 2 hours
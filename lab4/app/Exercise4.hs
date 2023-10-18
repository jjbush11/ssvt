module Exercise4 where

import Exercise3
import Test.QuickCheck
import Data.List

isSerial :: (Eq a) => [a] -> Rel a -> Bool
isSerial domain rel = all (hasRelation domain rel) domain
  where
    hasRelation :: (Eq a) => [a] -> Rel a -> a -> Bool
    hasRelation domain rel x = any (\y -> (x, y) `elem` rel) domain

prop_isSerial_returns_true_for_identity_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_true_for_identity_relation (NonEmpty domain) =
  let rel = [(x, x) | x <- domain]
  in isSerial domain rel === True

prop_isSerial_returns_false_for_empty_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_empty_relation (NonEmpty domain) =
  let rel = []
  in isSerial domain rel === False

prop_isSerial_returns_false_for_incomplete_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_incomplete_relation (NonEmpty domain) =
  let rel = [(x, y) | x <- domain, y <- domain, x < y]
  in isSerial domain rel === False

prop_isSerial_returns_false_for_asymmetric_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_asymmetric_relation (NonEmpty domain) =
  let rel = [(x, y) | x <- domain, y <- domain, x < y]
  in isSerial domain rel === (not $ all (\(x, y) -> (y, x) `notElem` rel) rel)

modularRelation :: Rel Int
modularRelation = [(x, y) | x <- [0 ..], y <- [0 ..], n <- [1 ..], x == y `mod` n]

-- If we look at the above mentioned definition for a modular relation.
-- The result is serial. The serial relationship that exists is the following:
-- modularRelationForNSerial :: Int -> Rel Int
-- modularRelationForNSerial n = [(x, x) | x <- [0..]]
-- For every element x, there is a relation (x,x) in the relation. Thus the relation is serial.

modularRelationForN :: Int -> Rel Int
modularRelationForN n = [(x, y) | x <- [0 .. 10], y <- [0 .. 10], x `mod` n == y `mod` n]

-- With these properties in place, you can use QuickCheck to generate a multitude of test cases and check whether the property holds true for all of them.
-- If QuickCheck doesn't find any counterexamples, it increases our confidence that the property is true (though it doesn't constitute a formal proof).

main :: IO ()
main = do

  quickCheck prop_isSerial_returns_true_for_identity_relation
  quickCheck prop_isSerial_returns_false_for_empty_relation
  quickCheck prop_isSerial_returns_false_for_incomplete_relation
  quickCheck prop_isSerial_returns_false_for_asymmetric_relation

  print $ take 10 modularRelation
  print $ take 30 $ modularRelationForN 2

  print $ isSerial [0 .. 10] (modularRelationForN 1)
  print $ isSerial [0 .. 10] (modularRelationForN 2)
  print $ isSerial [0 .. 10] (modularRelationForN 3)
  print $ isSerial [0 .. 10] (modularRelationForN 4)
  print $ isSerial [0 .. 10] (modularRelationForN 5)
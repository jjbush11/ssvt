module Exercise4 where

import Data.List
import Exercise3
import Test.QuickCheck

-- The isSerial function checks whether a relation is serial or not.
-- A relation R on a set A is called serial if for every element x in A, there is an element y in A such that xRy.
isSerial :: (Eq a) => [a] -> Rel a -> Bool
isSerial domain rel = all (hasRelation domain rel) domain
  where
    hasRelation :: (Eq a) => [a] -> Rel a -> a -> Bool
    hasRelation domain rel x = any (\y -> (x, y) `elem` rel) domain

-- This property tests wether isSerial always returns true for identity relations.
prop_isSerial_returns_true_for_identity_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_true_for_identity_relation (NonEmpty domain) =
  let rel = [(x, x) | x <- domain]
   in isSerial domain rel === True

-- This property tests wether isSerial always returns false for empty relations.
prop_isSerial_returns_false_for_empty_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_empty_relation (NonEmpty domain) =
  let rel = []
   in isSerial domain rel === False

-- This property tests wether isSerial always returns false for incomplete relations.
prop_isSerial_returns_false_for_incomplete_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_incomplete_relation (NonEmpty domain) =
  let rel = [(x, y) | x <- domain, y <- domain, x < y]
   in isSerial domain rel === False

-- This property tests wether isSerial always returns false for asymmetric relations.
prop_isSerial_returns_false_for_asymmetric_relation :: NonEmptyList Int -> Property
prop_isSerial_returns_false_for_asymmetric_relation (NonEmpty domain) =
  let rel = [(x, y) | x <- domain, y <- domain, x < y]
   in isSerial domain rel === not (all (\(x, y) -> (y, x) `notElem` rel) rel)

-- Define a modular relation for a specific n and domain.
-- This is in line with the relation R = {(x, y) | x = y(mod n)}.
modularRelationForN :: Int -> [Int] -> Rel Int
modularRelationForN n domain = [(x, y) | x <- domain, y <- domain, x `mod` n == y]

-- This property tests wether the modular relation is serial for a specific n and domain.
prop_modularRelationIsSerial :: [Int] -> Int -> Bool
prop_modularRelationIsSerial domain n = isSerial domain (modularRelationForN n domain)

-- With these properties in place, you can use QuickCheck to generate a multitude of test cases and check whether the property holds true for all of them.
-- If QuickCheck doesn't find any counterexamples, it increases our confidence that the property is true (though it doesn't constitute a formal proof).

main :: IO ()
main = do
  -- We test the properties for the isSerial function.
  putStrLn "Testing properties for isSerial function:"
  quickCheck prop_isSerial_returns_true_for_identity_relation
  quickCheck prop_isSerial_returns_false_for_empty_relation
  quickCheck prop_isSerial_returns_false_for_incomplete_relation
  quickCheck prop_isSerial_returns_false_for_asymmetric_relation

  -- isSerial always returns true for the modular relation R = {(x, y) | x = y(mod n)}.
  -- for the domain [0 .. 10] and 0 < n <= 5.
  -- So we could conclude that the relation is serial for all n <= 5.
  -- The same is probably be true for all n > 5, if the domain is large enough.
  -- However, due to computational constraints, we can't test this.
  quickCheck $ forAll (choose (1, 5)) $ \n ->
    prop_modularRelationIsSerial [0 .. 10] n

-- Indication of time spent: 120 minutes
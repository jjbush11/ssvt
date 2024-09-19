module Exercise7 where

import Exercise3
import Exercise5
import Exercise6 (isSymmetric, isTransitive)
import Test.QuickCheck

-- Yes, there is a difference between the symmetric closure of the transitive closure of a relation R
-- and the transitive closure of the symmetric closure of R.

-- For example, if we have the set {a, b} and the relationship {(a, b)}
-- The transitive closure of this relation remains the same, because there is no other relationship that can be added.
-- Thus the symmetric closure results in: {(a, b), (b, a)}

-- If we first take the symmetric closure, this results in the same set.
-- {(a, b), (b, a)}

-- However, if we then take the transitive closure of this set, we get:
-- {(a, b), (b, a), (a, a), (b, b)}

-- If we compare the two sets, we can see that they are not equal.
-- In the latter, the relationship (a, a) and (b, b) are added.

-- Below is the code to test this for this simple example.
-- {(1,2)} is used instead of {(a, b)} for simplicity.

simpleRel :: [(Int, Int)]
simpleRel = [(1, 2)]

symTransClosure :: (Ord a) => [(a, a)] -> [(a, a)]
symTransClosure xs = symClos (trClos xs)

transSymClosure :: (Ord a) => [(a, a)] -> [(a, a)]
transSymClosure xs = trClos (symClos xs)

-- We expected the two sets to be unequal, as in the example above.
-- However, in the case of the empty set, the following property doesn't hold.
-- It also doesn't hold for the sets [(1, 1)] and [(1,3),(3,1),(0,0)].
-- So, we came to the conclusion that the symTransClosure and transSymClosure
-- can be equal if and only if the original set:
-- 1. is empty
-- 2. contains already symmetrical pairs
-- 3. doesn't have any transitive pairs to add
prop_symTransUnequal :: (Ord a) => [(a, a)] -> Bool
prop_symTransUnequal xs = symTransClosure xs /= transSymClosure xs

main :: IO ()
main = do
  -- The result of the following is as expected: [(1, 2), (2, 1)]
  putStrLn "The symmetric closure of the transitive closure of our simple relation:"
  putStrLn $ show (symTransClosure simpleRel) ++ "\n"
  -- The result of this is also as expected from our answer above: [(1, 2), (2, 1), (1, 1), (2, 2)]
  putStrLn "The transitive closure of the symmetric closure of our simple relation:"
  putStrLn $ show (transSymClosure simpleRel) ++ "\n"
  -- We test the property for our simple relation
  -- As we expected, the property holds. Which means the two sets aren't equal.
  putStr "The property holds for our simple relation: "
  print $ prop_symTransUnequal simpleRel

  -- We also test this property with QuickCheck
  -- The result passes all tests if they already are symmetrical and transitive
  -- or if they are already symmetrical and no transitive pairs can be added.
  -- So sometimes 100 tests pass, but sometimes they don't.
  -- We skip the empty list, otherwise it would always fail.
  putStrLn "The property doesn't hold for all random generated relations:"
  quickCheck $ expectFailure $ forAll (arbitrary `suchThat` (not . null)) (\xs -> prop_symTransUnequal (xs :: [(Int, Int)]))

-- Time Spent: 45 minutes
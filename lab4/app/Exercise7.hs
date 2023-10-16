module Exercise7 where

import Exercise3
import Exercise5
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

symTransClosure :: [(Int, Int)] -> [(Int, Int)]
symTransClosure xs = symClos (trClos xs)

transSymClosure :: [(Int, Int)] -> [(Int, Int)]
transSymClosure xs = trClos (symClos xs)

-- We expect the two sets to be unequal
-- If they are equal, the test fails (and thus the property is false)
prop_symTransClosure :: [(Int, Int)] -> Bool
prop_symTransClosure xs = symTransClosure xs /= transSymClosure xs

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
  print $ prop_symTransClosure simpleRel

  -- We also test this property with QuickCheck
  -- The result passes all tests for lists of relations that are not empty
  -- TO-DO: Fix it for relations like [(1,1)]
  putStrLn "The property holds for random generated relations:"
  quickCheck $ forAll (arbitrary `suchThat` (not . null)) (\xs -> prop_symTransClosure (xs :: [(Int, Int)]))
module Exercise6 where

import Exercise3
import Exercise5
import Test.QuickCheck

-- The original relation should be a subset of the closure.
-- This can be used for both the symmetric and transitive closure.
-- It ensures that no elements are removed from the original relation.
prop_subset :: (Ord a) => (Rel a -> Rel a) -> Rel a -> Bool
prop_subset closureFunc rel = all (`elem` closure) rel
  where
    closure = closureFunc rel

-- For both the symmetric and transitive closure.
-- If a closure is applied twice, it should result in the same relation.
prop_idempotence :: (Ord a) => (Rel a -> Rel a) -> Rel a -> Bool
prop_idempotence closureFunc rel = closure == closureFunc closure
  where
    closure = closureFunc rel

-- For a relation to be symmetric, for every pair (x, y) in the relation, (y, x) should also be in the relation.
isSymmetric :: (Ord a) => Rel a -> Bool
isSymmetric rel = all (\(x, y) -> (y, x) `elem` rel) rel

-- A property which checks if the symmetric closure of a relation is symmetric.
prop_symClosSymmetry :: (Ord a) => Rel a -> Bool
prop_symClosSymmetry rel = isSymmetric closure
  where
    closure = symClos rel

-- For a relation to be transitive, if (a, b) and (b, c) are in the relation,
-- then (a, c) should also be in the relation.
isTransitive :: (Ord a) => Rel a -> Bool
isTransitive rel = all transitiveCheck rel
  where
    -- For each pair (a, b) in the relation, check if there exists a pair (b, c) in the relation.
    -- If there is, then (a, c) must also be in the relation.
    transitiveCheck (a, b) = all (\(x, y) -> x /= b || (a, y) `elem` rel) rel

-- A property which checks if the transitive closure of a relation is transitive.
prop_trClosTransitivity :: (Ord a) => Rel a -> Bool
prop_trClosTransitivity rel = isTransitive closure
  where
    closure = trClos rel

main :: IO ()
main = do
  -- For both the symmetric closure and the transitive closure,
  -- the tests for subset and idempotence pass.
  -- Meaning that the functions both return a superset of the original relation,
  -- and that applying the function twice results in the same relation.
  putStrLn "Testing for subset:"
  quickCheck (prop_subset symClos :: Rel Int -> Bool)
  quickCheck (prop_subset trClos :: Rel Int -> Bool)
  putStrLn "Testing for idempotence:"
  quickCheck (prop_idempotence symClos :: Rel Int -> Bool)
  quickCheck (prop_idempotence trClos :: Rel Int -> Bool)

  -- This test checks if the symmetric closure of a relation is symmetric.
  -- All tests passed, indicating that our symmetric closure function
  -- for the 100 tests correctly generates a symmetric relation.
  putStrLn "Testing for symmetry of symmetric closure:"
  quickCheck (prop_symClosSymmetry :: Rel Int -> Bool)

  -- This test checks if the transitive closure of a relation is transitive.
  -- All tests passed, so we can assume that our implementation
  -- for the 100 tests computed the transitive closure correctly.
  putStrLn "Testing for transitivity of transitive closure:"
  quickCheck (prop_trClosTransitivity :: Rel Int -> Bool)

-- Time Spent: 40 minutes
module Exercise6 where

import Exercise3
import Exercise4
import Exercise5
import Test.QuickCheck
import Data.List

-- The original relation should be a subset of the closure.
-- This can be used for both the symmetric and transitive closure.
-- It ensures that no elements are removed from the original relation.
prop_subset :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
prop_subset closureFunc rel = all (`elem` closure) rel
  where
    closure = closureFunc rel

-- For both the symmetric and transitive closure. 
-- If a closure is applied twice, it should result in the same relation.
prop_idempotence :: Ord a => (Rel a -> Rel a) -> Rel a -> Bool
prop_idempotence closureFunc rel = closure == closureFunc closure
  where
    closure = closureFunc rel

-- For every relation in the symmetric closure, its reverse should also be in the symmetric closure.
prop_symClosReflexivity :: (Ord b) => Rel b -> Bool
prop_symClosReflexivity rel = all (\(x, y) -> (y, x) `elem` closure) closure
  where
    closure = symClos rel

-- If (a, b) and (b, c) are in the relation, then (a, c) should be in the transitive closure.
prop_trClosTransitivity :: (Ord a) => Rel a -> Bool
prop_trClosTransitivity rel = all transitiveCheck rel
  where
    closure = trClos rel
    -- For each pair (x, y) in the relation, check if there exists a (y, z) in the relation.
    -- If there is, then (x, z) must be in the transitive closure.
    transitiveCheck (x, y) = 
      all (\(a, b) -> a /= y || (x, b) `elem` closure) rel

main :: IO ()
main = do
  quickCheck (prop_subset symClos :: Rel Int -> Bool)
  quickCheck (prop_subset trClos :: Rel Int -> Bool)
  quickCheck (prop_idempotence symClos :: Rel Int -> Bool)
  quickCheck (prop_idempotence trClos :: Rel Int -> Bool)
  quickCheck (prop_symClosReflexivity :: Rel Int -> Bool)
  quickCheck (prop_trClosTransitivity:: Rel Int -> Bool)
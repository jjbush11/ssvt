module Exercise2 where

import Data.List
import SetOrd
import Test.QuickCheck

-- The intersection means all the elements that are in both sets.
-- Instead of the list comprehension: [x | x <- a, x `elem` b]
-- we use a traversal method that should be more efficient.
-- We sort the lists first, and then traverse them in parallel.
-- If the elements are equal, we add them to the result and continue with the rest of the lists.
-- If the first element is smaller than the second, we continue with the rest of the first list.
-- Otherwise, we continue with the rest of the second list.
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set $ intersect (sort a) (sort b)
  where
    intersect [] _ = []
    intersect _ [] = []
    intersect (x : xs) (y : ys)
      | x == y = x : intersect xs ys
      | x < y = xs `intersect` (y : ys)
      | otherwise = (x : xs) `intersect` ys

-- The union means all the elements that are in either set.
-- We concatenate the lists and remove duplicates using nub.
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set $ nub (a ++ b)

-- The difference means all the elements that are in the first set, but not in the second.
-- We use the same traversal method as in setIntersection.
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set $ difference (sort a) (sort b)
  where
    difference [] _ = []
    difference xs [] = xs
    difference (x : xs) (y : ys)
      | x == y = difference xs ys
      | x < y = x : difference xs (y : ys)
      | otherwise = difference (x : xs) ys

main :: IO ()
main = do
  print "Hello World"
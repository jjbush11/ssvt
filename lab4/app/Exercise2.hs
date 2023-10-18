module Exercise2 where

import Data.List
import Exercise1
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

-- A property which checks if the intersection of two sets contains
-- only the elements that are in both sets.
prop_intersectionContainsOnly :: (Ord a) => Set a -> Set a -> Bool
prop_intersectionContainsOnly (Set a) (Set b) = case setIntersection (Set a) (Set b) of
  Set intersection -> all (`elem` a) intersection && all (`elem` b) intersection

-- A property which checks if the union of two sets contains all the elements of both sets.
prop_unionContainsAll :: (Ord a) => Set a -> Set a -> Bool
prop_unionContainsAll (Set a) (Set b) = case setUnion (Set a) (Set b) of
  Set union -> all (`elem` union) a && all (`elem` union) b

-- A property which checks if the difference of two sets contains
-- only the elements that are in the first set, but not in the second.
prop_differenceContainsOnly :: (Ord a) => Set a -> Set a -> Bool
prop_differenceContainsOnly (Set a) (Set b) = case setDifference (Set a) (Set b) of
  Set difference -> all (`elem` a) difference && all (`notElem` b) difference

main :: IO ()
main = do
  -- All the examples below return the expected answers.

  -- The union of [1, 2, 3] and [2, 3, 4] should be [1, 2, 3, 4]
  putStrLn "The union of [1, 2, 3] and [2, 3, 4] should be [1, 2, 3, 4]:"
  putStrLn $ show (setUnion (Set [1, 2, 3]) (Set [2, 3, 4])) ++ "\n"

  -- The intersection of [1, 2, 3] and [2, 3, 4] should be [2, 3]
  putStrLn "The intersection of [1, 2, 3] and [2, 3, 4] should be [2, 3]:"
  putStrLn $ show (setIntersection (Set [1, 2, 3]) (Set [2, 3, 4])) ++ "\n"

  -- The difference of [1, 2, 3] and [2, 3, 4] should be [1]
  putStrLn "The difference of [1, 2, 3] and [2, 3, 4] should be [1]:"
  putStrLn $ show (setDifference (Set [1, 2, 3]) (Set [2, 3, 4])) ++ "\n"

  -- Tests with QuickCheck
  putStrLn "Testing the intersection property with QuickCheck:"
  quickCheck (prop_intersectionContainsOnly :: Set Int -> Set Int -> Bool)

  putStrLn "Testing the union property with QuickCheck:"
  quickCheck (prop_unionContainsAll :: Set Int -> Set Int -> Bool)

  putStrLn "Testing the difference property with QuickCheck:"
  quickCheck (prop_differenceContainsOnly :: Set Int -> Set Int -> Bool)

-- All tests passed for all properties, thus we can be quite sure that our functions work as expected.

-- Time spent: 45 minutes

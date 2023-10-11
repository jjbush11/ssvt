module Exercise2 where

import Test.QuickCheck
import SetOrd
import Data.List

-- The implementations below are not that fast. 
-- We can fix this, by making this: 


-- The intersection means all the elements that are in both sets.
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set [x | x <- a, x `elem` b]

-- setIntersection :: Ord a => [a] -> [a] -> [a]
-- setIntersection a b = intersect (sort a) (sort b)
--   where
--     intersect [] _ = []
--     intersect _ [] = []
--     intersect (x:xs) (y:ys)
--       | x == y    = x : intersect xs ys
--       | x < y     = intersect xs (y:ys)
--       | otherwise = intersect (x:xs) ys

-- The union means all the elements that are in either set.
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set $ nub (a ++ b)

-- setUnion :: Ord a => [a] -> [a] -> [a]
-- setUnion a b = union (sort a) (sort b)
--   where
--     union [] ys = ys
--     union xs [] = xs
--     union (x:xs) (y:ys)
--       | x == y    = x : union xs ys
--       | x < y     = x : union xs (y:ys)
--       | otherwise = y : union (x:xs) ys


-- The difference means all the elements that are in the first set, but not in the second.
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set [x | x <- a, x `notElem` b]

-- setDifference :: Ord a => [a] -> [a] -> [a]
-- setDifference a b = difference (sort a) (sort b)
--   where
--     difference [] _ = []
--     difference xs [] = xs
--     difference (x:xs) (y:ys)
--       | x == y    = difference xs ys
--       | x < y     = x : difference xs (y:ys)
--       | otherwise = difference (x:xs) ys


main :: IO ()
main = do
  print "Hello World"
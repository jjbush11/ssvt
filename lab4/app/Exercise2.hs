module Exercise2 where

import Test.QuickCheck
import SetOrd
import Data.List

-- The intersection means all the elements that are in both sets.
setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set a) (Set b) = Set [x | x <- a, x `elem` b]

-- The union means all the elements that are in either set.
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set a) (Set b) = Set $ nub (a ++ b)

-- The difference means all the elements that are in the first set, but not in the second.
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = Set [x | x <- a, x `notElem` b]

main :: IO ()
main = do
  print "Hello World"
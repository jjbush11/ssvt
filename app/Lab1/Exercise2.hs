module Lab1.Exercise2 where

import Control.Monad
import Data.Char
import Data.List hiding (subsequences)
import GHC.Float
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.State
import Data.Maybe

-- Exercise 2
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequencesWithX ++ subsequencesWithoutX
  where
    subsequencesWithX = map (x:) (subsequences xs)
    subsequencesWithoutX = subsequences xs

prop_lengthOfPowerSet :: [a] -> Bool
prop_lengthOfPowerSet xs = 2 ^ length xs == length (subsequences xs)

-- Power sets are larger sets compared to the original set.
-- The power set has elements, where n is the number of members in a set.
-- The power set of a countable finite set is countable.
-- ​​The power set of an empty set has only one element.
-- Empty set and the set itself are the fixed elements of any power set.


-- Elements in Power Set

-- Uniqueness of Subsequences
unique :: (Foldable t, Eq a) => t a -> Bool
unique xs = all (unique' xs) xs
  where 
    unique' xs x = isJust $ find (x ==) xs


prop_uniqueness :: Eq a => [a] -> Bool
prop_uniqueness xs = unique $ subsequences xs


main :: IO ()
main = do
  print $ subsequences [1,2,3,4,5]
  -- quickCheck (prop_lengthOfPowerSet :: String -> Bool)

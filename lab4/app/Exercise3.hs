module Exercise3 where

import Data.List
import SetOrd

type Rel a = [(a, a)]

-- We have two different approaches to the symmetric closure.
-- The first one is a simple recursive function that adds the reverse of each pair to the relation.
-- There is however the possibility that the reverse of a pair is already in the relation.
-- Thus all duplicates are removed using nub.
symClos :: (Ord a) => Rel a -> Rel a
symClos = nub . reverse . symClos' []
  where
    symClos' acc [] = acc
    symClos' acc ((x, y) : xs) = symClos' ((y, x) : (x, y) : acc) xs

-- The second approach is to use the SetOrd module to make use of the Set data type.
-- We first convert the relation to a set, and then add the reverse of each pair to the set.
-- The result is then converted back to a list.
symClos' :: (Ord a) => Rel a -> Rel a
symClos' rel = case foldr insertSym (list2set rel) rel of
  Set x -> x
  where
    insertSym (x, y) = insertSet (y, x)

exampleRel :: Rel Int
exampleRel = [(1, 2), (2, 3), (3, 4)]

main :: IO ()
main = do
  -- The result of both functions is the same: [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
  -- However, the latter will probably be more efficient at determining the symmetric closure.
  -- This is because there is no need to sort the list and remove duplicates.
  putStrLn "The symmetric closure of the example relation, for both functions:"
  putStrLn $ "symClos:" ++ show (symClos exampleRel)
  putStrLn $ "symClos':" ++ show (symClos' exampleRel)

-- Time Spent: 60 minutes (15 just for fun)
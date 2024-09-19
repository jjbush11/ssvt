module Exercise5 where

import Data.List
import Exercise3

-- From the Lab 4 exercises:
-- For each pair (x, y) in the first relation r, and each pair (w, z) in the second relation s,
-- if y equals w, then include (x, z) in the result.
-- Nub is then used to remove duplicates from the result.
infixr 5 @@

(@@) :: (Eq a) => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- This function computes the transitive closure of a relation.
-- It does so by making use of the @@ operator defined above.
-- If the relation does not change after one step, it's the transitive closure.
-- Otherwise, just continue the process with the updated relation.
trClos :: (Eq a) => Rel a -> Rel a
trClos r
  | r == r' = r
  | otherwise = trClos r'
  where
    -- We perform a step of the transitive closure.
    -- Add the composed relation r @@ r to the original relation r and remove duplicates using nub.
    -- This means: r' = r U (r @@ r)
    r' = r `union` (r @@ r)

main :: IO ()
main = do
  -- The result of the following closure is as expected, namely:
  -- [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]
  putStrLn "The transitive closure of the example relation is:"
  print $ trClos exampleRel

-- Time spent: 30 minutes
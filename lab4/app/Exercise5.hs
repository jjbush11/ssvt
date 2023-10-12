module Exercise5 where

import Data.List
import Exercise3
import SetOrd
import Test.QuickCheck

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
    r' = nub (r ++ (r @@ r))

main :: IO ()
main = do
  print "Hello World"
  let exampleClosure = trClos exampleRel
  print exampleClosure
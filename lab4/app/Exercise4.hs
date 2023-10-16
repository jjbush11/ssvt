module Exercise4 where

import Test.QuickCheck
import SetOrd 
import Exercise3

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (hasRelation domain rel) domain  
  where
    hasRelation :: Eq a => [a] -> Rel a -> a -> Bool
    hasRelation domain rel x = any (\y -> (x,y) `elem` rel) domain  

modularRelation :: Rel Int
modularRelation = [(x,y) | x <- [0..], y <- [0..], n <- [1..], x == y `mod` n] 

-- If we look at the above mentioned definition for a modular relation.
-- The result is serial. The serial relationship that exists is the following: 
-- modularRelationForNSerial :: Int -> Rel Int
-- modularRelationForNSerial n = [(x, x) | x <- [0..]]
-- For every element x, there is a relation (x,x) in the relation. Thus the relation is serial.

modularRelationForN :: Int -> Rel Int
modularRelationForN n = [(x, y) | x <- [0..], y <- [0..], x `mod` n == y `mod` n]

-- With these properties in place, you can use QuickCheck to generate a multitude of test cases and check whether the property holds true for all of them. 
-- If QuickCheck doesn't find any counterexamples, it increases our confidence that the property is true (though it doesn't constitute a formal proof).

main :: IO ()
main = do 
  print $ take 10 modularRelation
  print $ take 30 $ modularRelationForN 2
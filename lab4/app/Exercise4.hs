module Exercise4 where

import Test.QuickCheck
import SetOrd 
import Exercise3

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (hasRelation domain rel) domain  
  where
    hasRelation :: Eq a => [a] -> Rel a -> a -> Bool
    hasRelation domain rel x = any (\y -> (x,y) `elem` rel) domain  

main :: IO ()
main = do 
  print "Hello World"
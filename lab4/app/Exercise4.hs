module Exercise4 where

import Test.QuickCheck
import SetOrd 
import Exercise3

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial = undefined

main :: IO ()
main = do 
  print "Hello World"
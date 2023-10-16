module Exercise6 where

import Exercise3
import Exercise4
import Exercise5
import Test.QuickCheck

prop_symclosure :: Ord b => Rel b -> Bool
prop_symclosure xs = null $ [(x, y) | (x, y) <- symClos xs, (y, x) `notElem` xs]

main :: IO ()
main = do
  print "Hello World"
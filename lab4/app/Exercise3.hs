module Exercise3 where

import SetOrd
import Test.QuickCheck

type Rel a = [(a, a)]

symClos :: (Ord a) => Rel a -> Rel a
symClos = undefined

main :: IO ()
main = do
  print "Hello World"
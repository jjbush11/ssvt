module Exercise5 where

import Test.QuickCheck
import SetOrd 
import Data.List
import Exercise3

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos = undefined

main :: IO ()
main = do 
  print "Hello World"
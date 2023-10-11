module Exercise3 where

import SetOrd
import Test.QuickCheck

type Rel a = [(a, a)]

symClos :: (Ord a) => Rel a -> Rel a
symClos = reverse . symClos' []
  where
    symClos' acc [] = acc
    symClos' acc ((x,y):xs) = symClos' ((y,x):(x,y):acc) xs

examleRel :: Rel Int
examleRel = [(1,2),(2,3),(3,4)]

main :: IO ()
main = do
  let examleClosure = symClos examleRel
  print examleClosure
  
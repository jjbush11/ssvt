module Exercise2 where

import Test.QuickCheck
import LTS

ltsGen :: Gen IOLTS
ltsGen = undefined

main :: IO ()
main = do 
  print "Hello World"
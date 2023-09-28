module Exercise2 where

import Test.QuickCheck
import LTS

-- A generator which will only generate invalid LTSs

-- A generator with n dependant on the depth of the tree
ltsGen :: Gen IOLTS
ltsGen = undefined

-- A generator which allows for loops in the LTS

main :: IO ()
main = do 
  print "Hello World"
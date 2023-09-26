module Exercise5 where

import Test.QuickCheck
import LTS 

testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT lts sut = undefined

main :: IO ()
main = do 
  print "Hello World"
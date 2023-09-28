module Exercise5 where

import Test.QuickCheck
import LTS 

-- An IOLTS which describes the correct behaviour of a door
doorModel :: IOLTS
doorModel = createIOLTS [(0, "?close", 1), (1, "!closed", 0), (1, "?open", 0), (1, "?lock", 2), (2, "!locked", 1), (2, "?unlock", 1), (2, "!unlocked", 1)]

-- I guess what they mean is that the IOLTS is the specification and the SUT is the implementation
-- Sut is one of the implementations of the door except for doorImpl1, which is correct
-- Test the SUT against the IOLTS
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT = undefined

main :: IO ()
main = do 
  print "Hello World"
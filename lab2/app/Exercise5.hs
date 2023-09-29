module Exercise5 where

import Test.QuickCheck
import LTS 

-- An IOLTS which describes the correct behaviour of a door
doorModel :: IOLTS
doorModel = createIOLTS [(0, "?close", 1), (1, "!closed", 0), (1, "?open", 0), (1, "?lock", 2), (2, "!locked", 1), (2, "?unlock", 1), (2, "!unlocked", 1)]

-- The IOLTS is the specification and the SUT is the implementation
-- Sut is one of the implementations of the door except for doorImpl1, which is correct
-- The SUT is tested against the specification
testLTSAgainstSUT :: IOLTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT lts sut = undefined

-- Unfortunately we did not have enough time to finish this exercise
-- This was due to the fact that we spent a lot of time on the previous exercises
-- Most of all because we didn't comprehend the definition of a quiescent trace at first
-- and it was difficult to find those traces, even given the code that was supplied. 
main :: IO ()
main = do 
  print "Hello World"
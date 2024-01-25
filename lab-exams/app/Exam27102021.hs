module Exam27102021 where

import Helpers
import LTS (IOLTS, createIOLTS, Trace, traces')


-- == Problem 3 ==
numK :: Int -> Int -> Int      
numK _ 0 = 0
numK _ 1 = 1
numK n k = sum $ map (\x -> numK (n-x) (k-1)) [0..n]

numKUnique :: Int -> Int -> Int
numKUnique = undefined 

-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper 

candyImpl :: LTS
candyImpl = ([1..4], ["?btn", "!choc", "!lic"], [(1, "?btn", 2), (2, "!choc", 3),
                                                       (2, "!lic", 4)], 1)
candyModel :: LTS
candyModel = ([1..4], ["?btn", "!lic"], [(1, "?btn", 2), (2, tau, 3),
                                                       (2, "!lic", 4)], 1)


-- Example 1: All states are reachable
iolts1 :: LTS
iolts1 = ([0..2], ["?input", "!output"], [(0, "?input", 1), (1, "!output", 2)], 0)

-- Example 2: State 2 is unreachable
iolts2 :: LTS
iolts2 = ([0..2], ["?input", "!output"], [(0, "?input", 1)], 0)

-- Example 3: State 1 is unreachable
iolts3 :: LTS
iolts3 = ([0..2], ["?input", "!output"], [(0, "!output", 2), (2, "?input", 0)], 0)

-- Example 4: All states are reachable, with loops
iolts4 :: LTS
iolts4 = ([0..2], ["?input", "!output"], [(0, "?input", 1), (1, "!output", 2), (2, "?input", 0)], 0)

-- Example 5: All states are reachable, with tau transitions
iolts5 :: LTS
iolts5 = ([0..2], ["?input", "!output"], [(0, "?input", 1), (1, "tau", 1), (1, "!output", 2)], 0)

lts1 :: LTS
lts1 = ([0..3], 
        ["?input1", "!output1"], 
        [
            (0, "?input1", 1),
            (1, "!output1", 0)
            -- States 2 and 3 are not reachable from initial state 0
        ], 
        0)


lts2 :: LTS
lts2 = ([0..4], 
        ["?start", "?process", "!done"], 
        [
            (0, "?start", 1),
            (1, "?process", 2),
            (2, "!done", 0)
            -- States 3 and 4 are not reachable from any of the other states
        ], 
        0)


lts3 :: LTS
lts3 = ([0..3], 
        ["?begin", "?advance", "!complete"], 
        [
            (0, "?begin", 1),
            (1, "?advance", 2),
            (2, "!complete", 1),
            (3, "?begin", 0)
            -- State 3 is only reachable from itself and can only transition to state 0, making it unreachable from the initial state
        ], 
        0)


-- == Problem 5 ==
corona1 r s x0 t = iterate ((s+).(r*)) x0 !! t

corona2 r s x0 t = (r^t-1)/(r-1)*s + r^t*x0

corona3 r s x0 t = foldr (-) x0 [s+r^d*x0 | d <- [0..t]]

corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)

corona5 r s x0 t = head.drop t $ iterate (\c -> s+r*c) x0

-- tracePairs :: LTS -> [Trace] -- [[Label]]
tracePairs (q, l, lt, q0) = traces' lt [([q0], [])]

main :: IO ()
main = do 
  -- Print all the results for applying unreachableStates to the above examples
  print $ hasUnreachableStates iolts1
  print $ hasUnreachableStates iolts2
  print $ hasUnreachableStates iolts3
  print $ hasUnreachableStates iolts4
  print $ hasUnreachableStates iolts5

  print $ hasUnreachableStates lts1
  print $ hasUnreachableStates lts2
  print $ hasUnreachableStates lts3
--   print $ tracePairs iolts1

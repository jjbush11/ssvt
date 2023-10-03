module Exercise4 where

import Test.QuickCheck
import LTS
import Data.List
import Exercise2

-- Function to find the target states of transitions with a given source state and label
findTransitions :: State -> Label -> [LabeledTransition] -> [State]
findTransitions s l transitions = [s'' | (s', l', s'') <- transitions, s == s', l == l']

-- Helper function to compute the set of states that can be reached from a given state and trace
-- It starts from the initial state given by the IOLTS
-- It then recursively calls itself with the next state and the remaining trace
-- All the results are concatenated and duplicates are removed
afterHelper :: IOLTS -> State -> Trace -> [State]
afterHelper lts s [] = [s]
afterHelper lts@(states, inputLabels, outputLabels, transitions, initialState) s (l:ls) =
  let nextStates = findTransitions s l transitions
      afterNextState = concatMap (\s' -> afterHelper lts s' ls) nextStates
  in nub afterNextState

-- The set of states that can be reached from a given state and trace
-- It starts from the initial state
-- This is passed to the helper function
after :: IOLTS -> Trace -> [State]
after (_,_,_,_,initialState) [] = [initialState]
after lts@(_,_,_,_,initialState) trace = afterHelper lts initialState trace

-- This property checks if the after function returns the initial state when an empty trace is given
prop_afterEmptyTrace :: IOLTS -> Property
prop_afterEmptyTrace lts@(_,_,_,_,initialState) = after lts [] === [initialState]

main :: IO ()
main = do 
  -- The after function behaves correctly for the counterModel
  quickCheck $ after counterModel ["coin"] == [2, 3]-- [2, 3]
  quickCheck $ after counterModel ["coin", "tea"] == [4] -- [4]

  -- Check the after function for a random IOLTS
  -- The after function behaves correctly for all random IOLTS that were tested.
  quickCheck $ forAll randomIOLTS prop_afterEmptyTrace


-- Time Spent: 4 hours
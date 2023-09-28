module Exercise4 where

import Test.QuickCheck
import LTS
import Data.List

-- Function to find the target states of transitions with a given source state and label
findTransitions :: State -> Label -> [LabeledTransition] -> [State]
findTransitions s l transitions = [s'' | (s', l', s'') <- transitions, s == s', l == l']

-- Helper function to compute the set of states that can be reached from a given state and trace
afterHelper :: IOLTS -> State -> Trace -> [State]
afterHelper lts s [] = [s]
afterHelper lts@(states, inputLabels, outputLabels, transitions, initialState) s (l:ls) =
  let nextStates = findTransitions s l transitions
      afterNextState = concatMap (\s' -> afterHelper lts s' ls) nextStates
  in nub afterNextState

-- 'after' function
after' :: IOLTS -> Trace -> [State]
after' (_,_,_,_,initialState) [] = [initialState]
after' lts@(_,_,_,_,initialState) trace = afterHelper lts initialState trace


main :: IO ()
main = do 
  print $ after' counterModel ["coin"] -- [2, 3]
  print $ after' counterModel ["coin", "tea"] -- [4]
module Exercise3 where

import LTS
import Test.QuickCheck

-- findTraces :: LTS -> [Trace]
-- findTraces (states, labels, transitions, initialState) = go initialState []
--   where
--     go :: State -> Trace -> [Trace]
--     go state trace
--       | state `notElem` states = [trace]
--       | otherwise = concatMap (\(_, label, nextState) -> go nextState (trace ++ [label])) outgoingTransitions
--       where
--         outgoingTransitions = filter (\(s, _, _) -> s == state) transitions


-- Figure out the suspension traces of the LTS
-- A trace is the path from the initial state to a final state and is defined as a list of labels
-- You have to account for tau transitions
-- You have to account for delta transitions
-- The traces function is given and should be used in the straces function
straces :: IOLTS -> [Trace]
straces = undefined

main :: IO ()
main = do
  print "Hello World"
  -- print $ findTraces counterModel
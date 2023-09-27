module Exercise1 where

import LTS
import Test.QuickCheck

-- The validity of an LTS can be checked, by checking the following properties:
-- 1. There are no empty states in Q
-- 2. There are no empty labels
-- 3. All transitions are valid and contain valid labels
-- 4. Check if the initial state is valid.
validateLTS :: IOLTS -> Bool
validateLTS (states, inputLabels, outputLabels, transitions, initialState) =
  validateStates states
    && validateTransitions transitions
  where
    validateStates :: [State] -> Bool
    validateStates = not . null

    validateTransitions :: [LabeledTransition] -> Bool
    validateTransitions = all isValidTransition

    isValidTransition :: LabeledTransition -> Bool
    isValidTransition (q, l, q') = q `elem` states && (l `elem` inputLabels || l `elem` outputLabels) && q' `elem` states

    validateInitialState :: State -> [State] -> Bool
    validateInitialState initialState states = initialState `elem` states

    deathLocks = undefined
    isDeadlock (q, l, q') states = q `notElem` states 

main :: IO ()
main = do
  print $ validateLTS counterModel
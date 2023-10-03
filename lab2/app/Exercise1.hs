module Exercise1 where

import LTS
import Test.QuickCheck
import Data.List


-- The validity of an LTS can be checked, by checking the following properties:
-- 1. States should be countable and non-empty
-- 2. Labels should be countable
-- 3. All transitions are valid and contain valid labels
-- 4. Check if the initial state is valid.

-- Invalid LTS factors
-- 1. States are infinite or empty
-- 2. Labels are infinite
-- 3. Transitions contain states that are not available in the set of states
-- 4. Transitions contain labels that are not available in the set of labels
-- 5. Initial state is not available in the set of states

validateLTS :: IOLTS -> Bool
validateLTS (states, inputLabels, outputLabels, transitions, initialState) =
  -- All the following properties follow from Chapter 3 Definition 1 by Tretmans
  validateStates states
  && isCountable states
  && isCountable inputLabels
  && isCountable outputLabels
  && validateTransitions transitions
  && validateInitialState initialState states
  -- All the following properties follow from Chapter 3 Definition 6 by Tretmans
  && validateLabels inputLabels outputLabels

  where
    -- The following properties follow from Chapter 3 Definition 1 by Tretmans
    validateStates :: [State] -> Bool
    validateStates = not . null

    -- It is not possible to test if a list is countable, since it can be infinite in Haskell.
    -- Thus we assume that the list is countable if the length of the list is less than the maximum Int value. (Not available for Interger type)
    isCountable :: [a] -> Bool 
    isCountable list = length list < x 
      where x = maxBound :: Int

    -- Check for all transitions if they are valid
    validateTransitions :: [LabeledTransition] -> Bool
    validateTransitions = all isValidTransition

    -- A transition is valid if T is an element of Q x L x Q
    isValidTransition :: LabeledTransition -> Bool
    isValidTransition (q, l, q') = q `elem` states && l `elem` (inputLabels ++ outputLabels) && q' `elem` states

    -- q_0 should be an element of the set of states
    validateInitialState :: State -> [State] -> Bool
    validateInitialState initialState states = initialState `elem` states

    -- The following property follows from Chapter 3 Definition 6 by Tretmans
    -- L_i and L_u are the sets of input and output labels respectively.
    -- They are disjoin: L_i ∩ L_u = ∅
    validateLabels :: [Label] -> [Label] -> Bool
    validateLabels inputLabels outputLabels = null (inputLabels `intersect` outputLabels)

-- A list of invalid factors for the LTS
invalidInitialState :: IOLTS 
invalidInitialState = (states, inputLabels, outputLabels, transitions, updatedInitialState)
  where
    (states, inputLabels, outputLabels, transitions, initialState) = counterModel
    updatedInitialState = 5

main :: IO ()
main = do
  print counterModel
  print $ validateLTS counterModel
  quickCheck $ expectFailure $ validateLTS invalidInitialState
  quickCheck $ validateLTS counterModel


-- Time Spent: 2 hour
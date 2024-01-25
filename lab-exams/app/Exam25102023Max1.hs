module Exam25102023Max1 where

import Test.FitSpec (Args (..), args)
import qualified Test.FitSpec as FS
import Test.FitSpec.Main (mainWith)
import LTS (IOLTS, createIOLTS, Trace)
import Data.List
import Test.QuickCheck (quickCheck, Gen, arbitrary, suchThat, forAll)

-- Helper functions
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- From Exercise 1
-- A generator which only allows for natural numbers to be generates
genPositiveNumbers :: Gen Int
genPositiveNumbers = abs <$> (arbitrary :: Gen Int) `suchThat` (> 0)

-- == Problem 3 ==
numNRIr :: Int -> Int
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2 ^ (n ^ 2 - 1) + 2 ^ (2 * n - 2) * numNRIr (n - 1)

prop_baseCase :: Int -> Bool
prop_baseCase n = n == 0 || n == 1 --> numNRIr n == 0

-- The following formula calculates the number of relations that are neither 
-- reflexive nor irreflexive
numNRIrFormula :: Int -> Int
numNRIrFormula n    | n <= 0 = 0
                    | otherwise = (2 ^ n - 2) * 2 ^ (n^ 2 - n)

-- This property checks if the formula is correct for all n > 0
prop_formula :: Int -> Bool
prop_formula n = numNRIr n == numNRIrFormula n

-- == Problem 4 ==
type State = Integer

type Label = String

type LabeledTransition = (State, Label, State)

type LTS = ([State], [Label], [LabeledTransition], State)

tau :: Label
tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper

delta :: Label
delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

dispenserImpl :: LTS
dispenserImpl = ([1 .. 4], ["?btn", "?switch", "!water"], [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)], 1)

dispenserModel :: LTS
dispenserModel = ([1 .. 4], ["?btn", "!water"], [(1, "?btn", 2), (2, "tau", 3), (2, "!water", 4)], 1)

order :: LTS
order =
  ( [0 .. 5],
    ["?coin", "?btn", "!food", "!drink", "!receipt"],
    [ (0, "?coin", 1),
      (1, "?coin", 2),
      (2, "!food", 3),
      (0, "?btn", 3),
      (3, "!receipt", 4),
      (0, "?coin", 5),
      (5, "!drink", 3)
    ],
    0
  )

dispenserImplIOLTS :: IOLTS
dispenserImplIOLTS = createIOLTS [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)]

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

-- Given an aribtrary trace, using the dispenserImpl, this function will return wether water is dipensed or not
-- It does this by checking if the dispenserImplIOLTS reaches state 4 (a.k.a. water dispensed), using above defined after function
superWater :: [Label] -> Bool 
superWater trace = 4 `elem` after dispenserImplIOLTS trace

-- == Problem 5 ==
-- r = expected number of people infected by 1 person in one day
-- s = constant infections per day indepent of r
-- x0 = infections on day 0
-- t = at day t
corona :: Float -> Float -> Float -> Int -> Float
corona r s x0 t = r ^ t * x0 + s * (r ^ t - 1) / (r - 1)

-- The amount of infections at day t is always non-negative
-- prop_nonNegative :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
-- prop_nonNegative corona r s x0 t = r >= 0 && s >= 0 && x0 >= 0 && t >= 0 --> corona r s x0 t >= 0

-- The amount of infections at day 0 is always equal to the amount of infections at day 0
prop_x0 :: (Float -> Float -> Float -> Int -> Float) -> Float -> Float -> Float -> Int -> Bool
prop_x0 corona r s x0 t = t == 0 --> corona r s x0 t == x0

-- A property which follows the recurrence equation as defined in the problem statement
-- Xt+1 −Xt = (R − 1) ·Xt + S
prop_recurrenceRel :: (Float -> Float -> Float -> Int -> Float) -> Float -> Float -> Float -> Int -> Bool
prop_recurrenceRel corona r s x0 t = r >= 0 && s >= 0 && x0 >= 0 && t >= 0 --> xtPlus1 - xt == (r - 1) * xt + s
  where
    xt = corona r s x0 t
    xtPlus1 = corona r s x0 (t + 1)

-- A list of above defined properties
propertiesCorona :: (Float -> Float -> Float -> Int -> Float) -> [FS.Property]
propertiesCorona corona =
  [ 
    FS.property $ prop_x0 corona,
    FS.property $ prop_recurrenceRel corona
  ]

testFitSpecCorona :: IO ()
testFitSpecCorona =
  mainWith
    args
      { names = ["corona r s x0 t"],
        nMutants = 4000,
        nTests = 4000,
        timeout = 0
      }
    (corona :: Float -> Float -> Float -> Int -> Float)
    propertiesCorona

main :: IO ()
main =
  do
    -- Problem 3
    -- Test the properties using QuickCheck 
    quickCheck $ forAll genPositiveNumbers prop_baseCase
    quickCheck $ forAll genPositiveNumbers prop_formula

    -- Problem 4 
    -- Test the superWater function using the dispenserModel
    print "The following two values should hold True"
    print $ superWater ["?btn", "!water"]
    print $ superWater ["?btn", "?btn", "!water"]


    -- Problem 5 
    -- Due to the nature of the corona function, the following executions of it will result in NaN
    -- This is because the function will try to divide by zero
    print "The following two values hold NaN"
    let xt = corona 1 0 0 0
    let xtPlus1 = corona 1 0 0 1
    print xt 
    print xtPlus1
    print $ xtPlus1 - xt == (1 - 1) * xt + 0

    -- Execute the FitSpec test for the corona function
    -- As explained above, the test will fail due to the fact that the corona function will try to divide by zero
    -- if the value of r is 1
    testFitSpecCorona
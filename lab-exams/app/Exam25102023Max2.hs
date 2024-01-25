module Exam25102023Max2 where

import Data.List
import Helpers
import LTS (IOLTS, createIOLTS, Trace)
import Test.FitSpec (Args (..), args)
import qualified Test.FitSpec as FS
import Test.FitSpec.Main (mainWith)
import Test.QuickCheck

-- == Problem 2 == 
-- Consider the properties of coreflexive transitive and asymmetric transitive for relation R on a domain A. 
-- Suppose relations are represented as lists of pairs: type Rel a = [(a,a)].
type Rel a = [(a,a)]

prop_incomparable :: [Int] -> Bool
prop_incomparable domain = compareRelationProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "incomparable"

-- a prop which checks if coreflexive is stronger than reflexive
prop_coreflexiveStronger :: [Int] -> Bool
prop_coreflexiveStronger domain = compareRelationProperties domain isCoreflexive isReflexive == "prop1 is stronger"

-- == Problem 3 ==
numNRIr :: Int -> Int
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2^(n^2-1) + 2^(2*n-2) * numNRIr (n-1)

-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper 
delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

dispenserImpl :: LTS
dispenserImpl = ([1..4], ["?btn", "?switch", "!water"], [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)], 1)

dispenserModel :: LTS
dispenserModel = ([1..4], ["?btn", "!water"], [(1, "?btn", 2), (2, "tau", 3), (2, "!water", 4)], 1)

dispenserImplIOLTS :: IOLTS
dispenserImplIOLTS = createIOLTS [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)]

ltsToIolts :: LTS -> IOLTS
ltsToIolts (states, labels, transitions, initial) = (states, labelIn, labelOut, transitions, initial)
    where labelIn = filter (\l -> head l == '?') labels
          labelOut = filter (\l -> head l == '!') labels

-- superWater given an arbitrary trace and using the above dispenserImpl LTS, returns wether water is dispensed
superWater :: [Label] -> Bool
superWater trace = after dispenserImplIOLTS trace == [4]

order :: LTS
order = ([0..5], 
        ["?coin", "?btn", "!food", "!drink", "!receipt"],
        [
            (0, "?coin", 1),
            (1, "?coin", 2),
            (2, "!food", 3),
            (0, "?btn", 3),
            (3, "!receipt", 4),
            (0, "?coin", 5),
            (5, "!drink", 3)
        ], 0)

orderAfter1 :: [State]
orderAfter1 = after (ltsToIolts order) ["?delta"]

orderAfter2 :: [State]
orderAfter2 = after (ltsToIolts order) ["?btn", "!receipt", "delta"]

orderAfter3 :: [State]
orderAfter3 = after (ltsToIolts order) ["?coin", "delta", "?coin", "!food"]

orderAfter4 :: [State]
orderAfter4 = after (ltsToIolts order) ["?coin", "delta", "!drink", "!receipt"]

orderAfter5 :: [State]
orderAfter5 = after (ltsToIolts order) ["delta", "?coin"]

-- == Problem 5 ==
  -- r = the expected number of infections generated by one infected person in a population by one infected individual per day
  -- s = the expected number of daily infections in the medical care system 
  -- x0 = initial number of infected people
  -- t = time in days
corona :: Double -> Double -> Double -> Int -> Double
corona r s x0 t = r^t*x0 + s * (r^t-1)/(r-1)


(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

prop_initialCondition :: (Double -> Double -> Double -> Int -> Double) -> Double -> Double -> Double -> Int -> Bool
prop_initialCondition f r s x0 t = f r s x0 0 == x0

-- properties ::  -> [Property]
properties :: (Double -> Double -> Double -> Int -> Double) -> [FS.Property]
properties corona =
  [ FS.property $ prop_initialCondition corona
    -- FS.property $ prop_reproductionNumberIncrease corona
  ]

testFitSpec :: IO ()
testFitSpec =
  mainWith
    args
      { names = ["corona r s x0 t"],
        nMutants = 4000,
        nTests = 4000,
        timeout = 0
      }
    (corona :: Double -> Double -> Double -> Int -> Double)
    properties

main :: IO ()
main =
  do
    -- Problem 3
    -- Test the property prop_incomparable using QuickCheck
    -- Checks if the incomparable property holds for a list of integers
    -- The length of the list has to be greater than 2 to be able to test for transitivity
    -- The list of integers should contain at least 2 elements and the size of the list should be less than 5
    -- The size is limited to 5 because the number of relations grows exponentially with the size of the list
    quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_incomparable

    -- Test if prop_coreflexiveStronger holds for a list of integers
    quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 5)) prop_coreflexiveStronger

    -- Problem 4 
    -- Test the superWater function using the dispenserModel
    print "The following two values should hold True"
    -- print $ superWater ["?btn", "!water"]
    -- print $ superWater ["?switch", "?btn", "!water"]
    print $ superWater ["btn", "water"]
    print $ superWater ["switch", "btn", "water"]

    -- Print the iolts of the dispenserImpl using the createIOLTS function
    print dispenserImplIOLTS

    -- Print the IOLTS of the dispenserModel using the ltsToIolts function
    print $ ltsToIolts dispenserImpl

    -- Print the orderAfter functions
    print orderAfter1
    print orderAfter2
    print orderAfter3
    print orderAfter4
    print orderAfter5


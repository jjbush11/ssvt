module Exam25012024Max where

import Helpers
import LTS (IOLTS, Trace, createIOLTS)
import Test.FitSpec (Args (..), args)
import qualified Test.FitSpec as FS
import Test.FitSpec.Main (mainWith)
import Test.QuickCheck
import Lecture2

-- == Problem 2 == 
-- Suppose relations are represented as lists of pairs: type Rel a = [(a,a)].
-- 1. b
-- Checks if a relation is coreflexive transitive using the functions provided in the Helpers module
isCoreflexiveTransitive :: Ord a => Rel a -> Bool
isCoreflexiveTransitive r  = isCoreflexive r && isTransitive r

-- Checks if a relation is asymmetric transitive using the functions provided in the Helpers module
isAsymmetricTransitive :: Ord a => Rel a -> Bool
isAsymmetricTransitive r = isAsymmetric r && isTransitive r

-- We proved that the relation is incomparable on the exam paper and now test it using QuickCheck
-- The property checks if the relation is incomparable using the compareRelationProperties function provided in the Helpers module
prop_incomparable :: [Int] -> Bool
prop_incomparable domain = compareRelationProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "incomparable"

rel :: Rel Int
rel = [(0,2),(3,1)]

minimalEquivalenceRel :: Rel Int
minimalEquivalenceRel = minimalEquivalence rel [0,1,2,3]

-- == Problem 3 ==
numNRIr :: Int -> Int
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2 ^ (n ^ 2 - 1) + 2 ^ (2 * n - 2) * numNRIr (n - 1)

prop_baseCase :: Int -> Bool
prop_baseCase n = n == 0 || n == 1 --> numNRIr n == 0

-- The following formula calculates the number of relations that are neither reflexive nor irreflexive
-- It was found on geeksforgeeks.org
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

tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper

delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper


juiceImpl :: LTS
juiceImpl = ([1 .. 3], ["?apple", "?orange", "!applejuice"], [(1, "?apple", 1), (1, "?orange", 2), (2, "!applejuice", 3)], 1)

juiceModel :: LTS
juiceModel = ([1 .. 5], ["?apple", "?orange", "!applejuice", "!orangejuice"], 
              [(1, "?apple", 2), (1, "?orange", 3), (2, "!applejuice", 4), (3, "!orangejuice", 5)], 1)

order :: LTS
order = ([0 .. 5], ["?card", "?coin", "?btn", "!snack", "!drink"],
    [ (0, "?card", 1), (1, "?btn", 2), (0, "?coin", 1), (1, "?btn", 4), (2, "!snack", 3), (4, "!drink", 5)], 0)

orderIOLTS :: IOLTS
orderIOLTS = createIOLTS [(0, "?card", 1), (1, "?btn", 2), (0, "?coin", 1), (1, "?btn", 4), (2, "!snack", 3), (4, "!drink", 5)]

orderAfter1 :: [State]
orderAfter1 = after orderIOLTS ["delta"]

orderAfter2 :: [State]
orderAfter2 = after orderIOLTS ["coin", "btn"]

orderAfter3 :: [State]
orderAfter3 = after orderIOLTS ["card", "delta", "btn", "snack"]

orderAfter4 :: [State]
orderAfter4 = after orderIOLTS ["card", "btn", "drink", "delta"]

orderAfter5 :: [State]
orderAfter5 = after orderIOLTS ["delta", "coin", "delta", "btn", "delta"]

-- Converts the juiceImpl LTS to an IOLTS
juiceIOLTS :: IOLTS
juiceIOLTS = createIOLTS [(1, "?apple", 1), (1, "?orange", 2), (2, "!applejuice", 3)]

juiceDispenser :: [Label] -> Bool
juiceDispenser trace = after juiceIOLTS trace == [3]

-- == Problem 5 ==
corona r s x0 t = r^t*x0 + s * r^t/r

-- On day 0, the number of infected people is equal to the initial condition
prop_initialCondition :: (Double -> Double -> Double -> Int -> Double) -> Double -> Double -> Double -> Int -> Bool
prop_initialCondition f r s x0 t = f r s x0 0 == x0

-- The number of infected people should increase every day
prop_increasing :: (Double -> Double -> Double -> Int -> Double) -> Double -> Double -> Double -> Int -> Bool
prop_increasing f r s x0 t = r > 0 || s > 0 --> f r s x0 (t+ 1) >= f r s x0 t

-- The number of infected people should adhere to the recurrence equation provided
prop_adheresToRecurrence :: (Double -> Double -> Double -> Int -> Double) -> Double -> Double -> Double -> Int -> Bool
prop_adheresToRecurrence f r s x0 t = f r s x0 (t+ 1) - f r s x0 t == (r - 1) * f r s x0 t + s

properties :: (Double -> Double -> Double -> Int -> Double) -> [FS.Property]
properties corona =
  [ 
    FS.property $ prop_initialCondition corona,
    FS.property $ prop_increasing corona,
    FS.property $ prop_adheresToRecurrence corona
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

-- 3. 
-- Equivalent mutants are alterations that can lead to semantically equivalent programs:
-- e.g. x + y = x (--y) 
-- Eq. mutants thus do not change the semantics of the program and not assert the correctness of the program
-- Detection might be possible by seeing if the performance of the program stays the same. 
-- And according to the MutantBench paper, one can use a dataset to 'automatically' detect equivalent mutants

-- 4. 
-- Code coverage should be used to express the amount of code that is executed by the tests.
-- Whilst mutation coverage, coverst both the execution and assertion of tests. 
-- So a 100% code coverage doesn't mean that all is asserted, and shouldn't be used as the go-to 
-- way to measure effectiveness, rather mutation coverage (100% means no holes in the test suite). 


main :: IO ()
main =
  do
    -- Problem 2
    print "Problem 2"
    print minimalEquivalenceRel
    -- Problem 3
    -- Test the property prop_incomparable using QuickCheck
    -- Checks if the incomparable property holds for a list of integers
    -- The length of the list has to be greater than 2 to be able to test for transitivity
    -- The size is limited to 10 because the number of relations grows exponentially with the size of the list
    -- All 100 tests pass, which means that the property holds for all lists of integers with a length greater than 2 and less than 10
    -- We can assume that the property always holds, but can't be sure because we can't test all possible lists of integers
    quickCheck $ forAll (listOf1 (arbitrary :: Gen Int) `suchThat` (\l -> length l > 2 && length l < 10)) prop_incomparable

    -- Problem 3
    -- Test the properties using QuickCheck 
    -- They hold for all values bigger than 0
    quickCheck $ forAll (arbitrary `suchThat` (>0)) prop_baseCase
    quickCheck $ forAll (arbitrary `suchThat` (>0)) prop_formula

    -- Problem 4
    -- -- Test the superWater function using the dispenserModel
    print "The following values should hold True"
    print $ juiceDispenser ["apple", "orange", "applejuice"]
    print $ juiceDispenser ["apple", "apple", "orange", "applejuice"]

    -- Print the orderAfter functions
    print orderAfter1
    print orderAfter2
    print orderAfter3
    print orderAfter4
    print orderAfter5

    -- Problem 5
    -- Test the corona function using FitSpec
    testFitSpec
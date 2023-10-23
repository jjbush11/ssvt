module Exam01022023
where

import Data.Foldable
import Data.Maybe
import GHC.Unicode (wgencat)
import System.Random
import Test.QuickCheck
import Data.List
import qualified Test.FitSpec as FS
import Test.FitSpec ( Args(..), args )
import Test.FitSpec.Main (mainWith)

-- == Problem 3 ==

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- From Exercise 1
-- A generator which only allows for natural numbers to be generates
genPositiveNumbers :: Gen Integer
genPositiveNumbers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

numS :: Integer -> Integer
numS 0 = 1
numS n = 2^n * numS (n-1)

-- https://www.geeksforgeeks.org/number-symmetric-relations-set/
numS' :: Integer -> Integer
numS' 0 = 1
numS' n = 2^((n * (n + 1)) `div` 2)

numSProp1 :: Integer -> Property
numSProp1 n = n >= 0 ==> numS n == numS' n

-- Does not hold

-- numSProp2 :: Integer -> Property
-- numSProp2 n =
--   n >= 0 ==> numS n == product [1..n+1] `div` 2

-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
tau = "tau" -- Assume tau behaves as it is defined in the Tretmans paper

coffeeImpl :: LTS
coffeeImpl = ([1..4], ["?btn", "?btn_milk", "!espresso", "!latte"],
    [(1, "?btn", 2), (1, "?btn_milk", 1), (2, "!espresso", 3), (2, "!latte", 4)], 1)
coffeeModel :: LTS
coffeeModel = ([1..4], ["?btn", "!espresso", "!latte"],
    [(1, "?btn", 2), (2, "!espresso", 3), (2, "!latte", 4)], 1)

drinks :: LTS
drinks = ([1..6], ["?btn", "!beer", "!cola"],[(1, "?btn", 2), (2, "!beer", 3), (1, "?btn", 4), (4, "?btn", 5), (5, "!cola", 6)], 1)


-- after function
-- write a function that given a trace on coffeeImpl returns wether a latte is made or not
-- quantumCoffee :: [Label] -> Bool
-- quantumCoffee trace = 4 `elem` after coffeeImpl trace


-- after :: LTS -> [Label] -> [State]
-- after (states, _, transitions, start) [] = [start]
-- after (states, actions, transitions, start) (label:labels) =
--   let nextStates = [to | (from, action, to) <- transitions, from `elem` afterStates, action == label]
--       afterStates = after (states, actions, transitions, start) labels
--   in if null nextStates then [] else nextStates

-- == Problem 5 ==

-- Problem 5 (20p) The spreading of a contagious disease occurs in two ways: by community transmission,
-- i.e., from ordinary social activities (in which individuals are assumed to freely and randomly interact
-- with each other), and by nosocomial transmission, i.e., contact between infected patients and medical
-- staff. Community transmission is unconstrained: the number of new infections depends linearly on the
-- current number of infections, whereas nosocomial transmission is limited: the number of new infections
-- is insensitive to the number of infections in the population.

-- Let Xt be the number of new infections recorded at day t = 0, 1, 2, . . ., let R be the expected number
-- of infections generated in the population by one infected individual in one day, and S the expected
-- number of daily infections in the medical care system in the following recurrence equation: Xt+1 −Xt =
-- (R − 1) ·Xt + S. Consider a corona implementation that computes the number of new infections at day
-- t given X0, R, S.,

-- r = expected number of people infected by 1 person in 1 day
-- s = constant infections per day indepent of r
-- x0 = infections on day 0
-- t = number of days

corona :: Int -> Int -> Int -> Int -> Int
corona r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)

prop_corona :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_corona corona r s x0 t = undefined

prop_nonNegative :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_nonNegative corona r s x0 t = r >= 0 && s >= 0 && x0 >= 0 && t >= 0 --> corona r s x0 t >= 0

prop_x0 :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_x0 corona r s x0 t = t == 0 --> corona r s x0 t == x0

-- Xt+1 −Xt = (R − 1) ·Xt + S
prop_recurrenceRel :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_recurrenceRel corona r s x0 t = r > 0 && s > 0 && x0 > 0 && t > 0 --> xtPlus1 - xt == (r-1) * xt + s
  where 
    xtPlus1 = corona r s x0 (t+1)
    xt = corona r s x0 t

-- increment ipv positive? 
prop_r_positive :: (Int -> Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Bool
prop_r_positive f r s x0 t = r > 1 && s >=0 && t > 0 --> f r s x0 t > f r s x0 (t-1)

propertiesCorona :: (Int -> Int -> Int -> Int -> Int) -> [FS.Property]
propertiesCorona corona =
  [
    FS.property $ prop_x0 corona,
    FS.property $ prop_nonNegative corona,
    FS.property $ prop_recurrenceRel corona,
    FS.property $ prop_r_positive corona
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
    (corona :: Int -> Int -> Int -> Int -> Int)
    propertiesCorona


-- Main
main :: IO ()
main = do
    -- quickCheck $ forAll genPositiveNumbers numSProp1
    -- quickCheck $ forAll genPositiveNumbers numSProp2
    quickCheck numSProp1
    -- quantumCoffee ["?btn", "!latte"] == True
    print $ corona 1 1 0 10
    print $ corona 1 1 1 10
    print $ corona 0 0 1 10 -- returns 1 r 0?
    print $ corona 1 0 1 10 -- returns 1
    testFitSpecCorona
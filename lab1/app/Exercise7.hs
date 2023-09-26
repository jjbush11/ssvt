{-# LANGUAGE InstanceSigs #-}

module Exercise7 where

import Data.List
import Lecture3
import SetOrd
import Test.QuickCheck

-- Exercise 7

-- A function which determines the amount of elements in a Set
sizeSet :: Set a -> Int
sizeSet (Set xs) = length xs

-- A function which determines the union of a list of Sets
unionSets :: Ord a => [Set a] -> Set a
unionSets = foldr unionSet emptySet

-- Counts all the subformulas of a formula
nsub :: Form -> Int
nsub form = sizeSet $ nsub' form
  where
    nsub' :: Form -> Set Form
    nsub' formula =
      case formula of
        Prop _ -> Set [formula]
        Neg f -> nsub'' [f]
        Cnj fs -> nsub'' fs
        Dsj fs -> nsub'' fs
        Impl f1 f2 -> nsub'' [f1, f2]
        Equiv f1 f2 -> nsub'' [f1, f2]
      where
        -- This function takes a list of formulas and returns a set of all subformulas
        -- It will recursively call itself on the subformulas of the formulas in the list
        nsub'' :: [Form] -> Set Form
        nsub'' = unionSets . (Set [formula] :) . map nsub'

arbitraryPositiveInt :: Int -> Gen Int
arbitraryPositiveInt n = choose (1, n :: Int)

-- A generator for formulas
-- The idea was to generate a formula based on n, the amount of subformulas it contains
-- Unfortunately, this seemed rather difficult to implement
-- It does however generate formulas with a random amount of subformulas
-- The size parameter is used to limit the depth of recursion
instance Arbitrary Form where
  arbitrary :: Gen Form
  arbitrary = sized $ \n -> do
    positiveIntegers <- vectorOf (n + 1) $ arbitraryPositiveInt (n + 1)
    genForm positiveIntegers n

genForm :: [Int] -> Int -> Gen Form
genForm positiveIntegers 0 = Prop <$> elements positiveIntegers
genForm positiveIntegers n =
  oneof
    [ Prop <$> elements positiveIntegers,
      Neg <$> arbitrary,
      Cnj <$> formList,
      Dsj <$> formList,
      Impl <$> subForm <*> subForm,
      Equiv <$> subForm <*> subForm
    ]
  where
    -- The size parameter is halved at each level of recusion to limit the depth of recursion.
    -- TO-DO: make n based on the amount of subformulas
    subForm = genForm positiveIntegers (n `div` 2)
    -- We assume that the amount of subformulas for a conjunction and disjunction is 2
    formList = vectorOf 2 subForm

-- Time Spent on generator: 30 minutes

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1, f2]) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet (unionSet (Set [f]) (sub f1)) (sub f2)

-- A fomula based on form3, but with a formula added to one of the conjunctions
-- It should return the same amount of subformulas as form3 + 1
form4 :: Form
form4 = Impl (Cnj [Impl p q, Impl q r, Impl r p]) (Impl p r)

-- A fomula based on form4, but the conjunctions are replaced with disjunctions
-- It should return the same amount of subformulas as form4
form5 :: Form
form5 = Impl (Dsj [Impl p q, Impl q r, Impl r p]) (Impl p r)

-- Test sub for predefined formulas
-- The following formulas are based on the formulas from Lecture 3
-- The amount of subformulas for each formula is calculated by hand
-- The size of the set of subformulas should be equal to the expected amount of subformulas
prop_form1_sub :: Bool
prop_form1_sub = 7 == sizeSet (sub form1)

prop_form2_sub :: Bool
prop_form2_sub = 7 == sizeSet (sub form2)

prop_form3_sub :: Bool
prop_form3_sub = 8 == sizeSet (sub form3)

props_sub :: [Bool]
props_sub = [prop_form1_sub, prop_form2_sub, prop_form3_sub]

-- This property checks the amount of subformulas of a predefined formula
-- It ensures that a formula with multiple conjunctions returns the correct amount of subformulas
prop_multipleConjunctive :: Bool
prop_multipleConjunctive = 9 == sizeSet (sub form4)

-- This property checks the amount of subformulas of a predefined formula
-- It ensures that a formula with multiple disjunctions returns the correct amount of subformulas
prop_multileDisjunctive :: Bool
prop_multileDisjunctive = 9 == sizeSet (sub form5)

-- Assert that the amount of subformulas of form 1 is 7
prop_form1 :: Bool
prop_form1 = 7 == nsub form1

-- Assert that the amount of subformulas of form 2 is 5
prop_form2 :: Bool
prop_form2 = 7 == nsub form2

-- Assert that the amount of subformulas of form 3 is 9
prop_form3 :: Bool
prop_form3 = 8 == nsub form3

-- Assert that the amount of subformulas of form 4 is 10
prop_form4 :: Bool
prop_form4 = 9 == nsub form4

-- Assert that the amount of subformulas of form 5 is 10
prop_form5 :: Bool
prop_form5 = 9 == nsub form5

props_nsub :: [Bool]
props_nsub = [prop_form1, prop_form2, prop_form3, prop_form4, prop_form5]

main :: IO ()
main = do
  -- Generate and print a small random formula of type Form
  formula <- generate $ resize 2 arbitrary :: IO Form
  print "An example of a generated formula:"
  print formula
  -- Exercise 7.1
  -- We can check if the sub implementation is correct by testing it on predefined formulas.
  -- The size of the set of subformulas should be equal to the expected amount of subformulas.
  -- The tests pass as expected, but we will later on show that the sub function doesn't work for all formulas.
  mapM_ quickCheck props_sub
  -- The following two properties test formulas with multiple conjunctions and disjunctions
  -- The two properties fail, with the following error messages:
  -- +++ OK, failed as expected. (after 1 test):
  -- Exception:
  --   app/Exercise7.hs:(69,1)-(74,70): Non-exhaustive patterns in function sub
  -- This is because the sub function doesn't take into account that a conjunction or disjunction can have more than 2 subformulas.
  -- The sub function doesn't take into account that a conjunction and disjunction can have more than two subformulas.
  -- This could be fixed by applying the sub function recursively to each subformula in the list and then combining the results into a single set, ensuring it handles any number of subformulas.
  quickCheck $ expectFailure prop_multipleConjunctive
  quickCheck $ expectFailure prop_multileDisjunctive
  -- Exercise 7.2
  -- The following properties test the amount of subformulas of predefined formulas
  -- The result of the tests pass as expected
  -- This proves that the nsub function works correctly for the predefined formulas
  -- It doesn't however prove that the nsub function works correctly for all formulas
  -- The idea was to generate a formula based on n, the amount of subformulas it would contain.
  -- Then we would check if nsub returns the correct amount of subformulas, which should've been equal to n.
  -- As explained above this proved to be rather difficult to implement.
  mapM_ quickCheck props_nsub
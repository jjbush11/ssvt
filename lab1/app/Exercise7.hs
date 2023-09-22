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
        Cnj fs ->  nsub'' fs
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

-- A fomula based on form3, but with a formula added to one of the conjunctions
-- It should return the same amount of subformulas as form3 + 1
form4 :: Form
form4 = Impl (Cnj [Impl p q, Impl q r, Impl r p]) (Impl p r)

main :: IO ()
main = do
  -- Generate and print a small random formula of type Form
  formula <- generate $ resize 2 arbitrary :: IO Form
  print formula
  print $ nsub formula
  print $ nsub form1
  print $ nsub form2
  print $ nsub form3
  print $ nsub form4
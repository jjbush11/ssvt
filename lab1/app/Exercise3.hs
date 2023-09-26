{-# LANGUAGE InstanceSigs #-}

module Exercise3 where

import Data.List
import Lecture2
import Test.QuickCheck

-- A datatype for propositions
-- The first argument is the name of the proposition
-- The second argument is the function which determines if the proposition holds for a given integer
-- This allows us to easily define properties and order them by strength
data Prop = Prop
  { name :: String,
    prop :: Int -> Bool
  }

-- An instance of the Show typeclass for Prop
-- This allows us to print the name of a proposition
instance Show Prop where
  show :: Prop -> String
  show (Prop name _) = name

-- Property which checks if a integer is even and greater then three:
prop1 :: Prop
prop1 = Prop "even x && x > 3" (\x -> even x && x > 3)

-- Property which checks if a integer is even or greater then three:
prop2 :: Prop
prop2 = Prop "even x || x > 3" (\x -> even x || x > 3)

-- Property which checks if a integer is even and greater then three or even:
prop3 :: Prop
prop3 = Prop "(even x && x > 3) || even x" (\x -> (even x && x > 3) || even x)

-- Property which checks if a integer is even:
prop4 :: Prop
prop4 = Prop "even" even

-- A list of all the properties
props :: [Prop]
props = [prop1, prop2, prop3, prop4]

-- A function which sorts a list of properties by descending strength
-- The first argument is the list of properties
-- The second argument is the domain to test the properties on
-- It does so by using sortBy and the weaker function
-- The sortBy function sorts the list of properties by making use of the weaker function
-- The weaker function is defined in Lecture2.hs
-- It takes a list of integers and two propositions
-- It returns true if the first proposition is weaker then the second proposition
-- It returns false if the first proposition is stronger then the second proposition
orderProps :: [Prop] -> [Int] -> [Prop]
orderProps ps i = sortBy (\p q -> compare (weaker i (prop p) (prop q)) False) ps

-- The ordered properties from strongest to weakest, tested on the domain [-10..10]
orderedProps :: [Prop]
orderedProps = orderProps props [(-10) .. 10]

-- Exercise 3
main :: IO ()
main = do
  -- Print the ordered properties from strongest to weakest
  -- The result of this is:
  -- even x && x > 3
  -- even
  -- (even x && x > 3) || even x
  -- even x || x > 3
  -- The following order is correct, because:
  -- The first property is the strongest, because it is the most specific.
  -- The second property is less specific, because it is a subset of the first property.
  -- The third property is less specific, because it is a subset of the first and second property.
  -- The fourth property is the weakest, because it is the most general.
  mapM_ print orderedProps

-- Time spent: 1 hour
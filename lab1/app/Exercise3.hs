{-# LANGUAGE InstanceSigs #-}

module Exercise3 where

import Data.List
import Lecture2
import Test.QuickCheck

data Prop = Prop
  { name :: String,
    prop :: Int -> Bool
  }

instance Show Prop where
  show :: Prop -> String
  show (Prop name _) = name

prop1 :: Int -> Bool
prop1 x = even x && x > 3

prop2 :: Int -> Bool
prop2 x = even x || x > 3

prop3 :: Int -> Bool
prop3 x = (even x && x > 3) || even x

prop4 :: Int -> Bool
prop4 x = even x && x > 3

prop5 :: Int -> Bool
prop5 = even

propDef1 :: Prop
propDef1 = Prop "even x && x > 3" (\x -> even x && x > 3)

propDef2 :: Prop
propDef2 = Prop "even x || x > 3" (\x -> even x || x > 3)

propDef3 :: Prop
propDef3 = Prop "(even x && x > 3) || even x" (\x -> (even x && x > 3) || even x)

propDef4 :: Prop
propDef4 = Prop "even x && x > 3" (\x -> even x && x > 3)

propDef5 :: Prop
propDef5 = Prop "even" even

propDefs :: [Prop]
propDefs = [propDef1, propDef2, propDef3, propDef4, propDef5]

-- Test for a single if it is stronger than the other
testStronger :: Prop -> Prop -> [Int] -> Bool
testStronger p q i = stronger i (prop p) (prop q)

-- Order the properties by strength
orderProps :: [Prop] -> [Int] -> [Prop]
orderProps ps i = sortBy (\p q -> compare (stronger i (prop p) (prop q)) False) ps

orderedProps :: [Prop]
orderedProps = orderProps propDefs [(-10) .. 10]

-- Exercise 3
main :: IO ()
main = do
  print orderedProps
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Exercise1 where

import Lecture2
import SetOrd
import System.Random
import Test.QuickCheck

-- A random generator for sets, using System.Random.
generateRandomSet :: IO (Set Int)
generateRandomSet = do
  -- Generate a random list of integers, using the generator from Lecture2.hs.
  -- And use list2set to convert the list to a set.
  list2set <$> genIntList

-- A generator using the Arbitrary typeclass from QuickCheck.
instance Arbitrary (Set Int) where
  arbitrary :: Gen (Set Int)
  arbitrary = do
    -- It generates a list of integers.
    -- And is then converted to a Set using list2set.
    list2set <$> arbitrary

main :: IO ()
main = do
  -- A random set from the generator from scratch is:
  putStrLn "Random set from generator from scratch:"
  randomSet <- generateRandomSet
  print randomSet

  -- A random set from the Arbitrary typeclass is:
  putStrLn "Random set from Arbitrary typeclass:"
  randomSet2 <- generate (arbitrary :: Gen (Set Int))
  print randomSet2
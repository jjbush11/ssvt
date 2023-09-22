module Exercise1 where

import Test.QuickCheck

-- Exercise 1
-- A generator which only allows for natural numbers to be generated
-- The precondition is placed here, so the properties can be as general as possible
genNaturalNumbers :: Gen Integer
genNaturalNumbers = (arbitrary :: Gen Integer) `suchThat` (>= 0)

-- https://www.wikiwand.com/en/Factorial
factorial :: Integer -> Integer
factorial n
  | n == 0 = 1
  | n >= 0 = n * factorial (n - 1)
  | otherwise = error "n is not allowed to be negative"

-- Test if the base case of the factorial function is correct
-- It should return 1
prop_factorialBase :: Bool
prop_factorialBase = factorial 0 == 1

-- Test if factorial increments correctly.
-- This is done by calculating factorial n, multiplying by (n + 1) and confirming the value is
-- the same as factorial (n + 1)
prop_factorialIncrement :: Integer -> Bool
prop_factorialIncrement n = factorial n * (n + 1) == factorial (n + 1)

-- Helper function which checks if a number is divisible by another number
isDivisableByX :: Integer -> Integer -> Bool
isDivisableByX n x = n `mod` x == 0

-- Test if the value returned by factorial is divisible by all values between 1 and n
prop_divisableBySmallerThanN :: Integer -> Bool
prop_divisableBySmallerThanN n = all (isDivisableByX (factorial n)) [1 .. n]

main :: IO ()
main = do
-- Test: prop_factorialBase
-- Test: prop_factorialIncrement
-- output: +++ OK, passed 1 test.
-- output: +++ OK, passed 100 tests.

-- So we would like to prove the following:
-- 1 * ... * n = factorial (n)
-- We test the base case, which returns 1
-- Then we test the induction step factorial n * (n + 1) = factorial (n + 1)
-- This also passed for 100 tests.
-- Thus all the cases of the factorial function are covered and work correctly.
-- Unfortunately, these 100 tests don't cover the whole domain (all natural numbers).
-- To do that, we would instead have to do a mathematical proof by induction.
  quickCheck prop_factorialBase
  quickCheck (forAll genNaturalNumbers prop_factorialIncrement)

-- Test: prop_divisableBySmallerThanN
-- output: +++ OK, passed 100 tests.
-- We already tested if our function is working for a large set of natural numbers.(*)
-- In this Test we want to proof the output of the function by checking if it is divisable by numbers lower then n
-- This also passed 100 test.
-- This property suggest that the facorial function correctly computes the factorial of n.
-- Just like previous test it doesn't dover the whole domain of all natural numbers.
  quickCheck prop_divisableBySmallerThanN
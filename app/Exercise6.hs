module Exercise6 where

import Data.List
import Test.QuickCheck

-- Code from lab0
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

-- A version that uses primes and skips all numbers before n.
primesFromN :: Integer -> [Integer]
primesFromN n
  | n > 1 = filter prime [n ..]
  | otherwise = error "n needs to be bigger than 1"

-- This function returns the following in form of a tuple with three members
-- 1. The first prime number, from which another 100 should be taken.
-- 2. The sum of these 101 prime numbers
-- 3. The list of 101 prime numbers including the first prime number
consecutive101Prime' :: [Integer] -> (Integer, Integer, [Integer])
consecutive101Prime' remainingPrimes@(x : xs)
  | prime sum101Primes = (x, sum101Primes, listOf101Primes)
  | otherwise = consecutive101Prime' xs
  where
    listOf101Primes = take 101 remainingPrimes
    sum101Primes = sum listOf101Primes

-- This function calls above mentioned function and only returns the sum of 101 primes
consecutive101Prime :: Integer
consecutive101Prime = case consecutive101Prime' primes of
  (_, sumOf101Primes, _) -> sumOf101Primes

-- We test if the answer is correct by looking at the following properties:
-- 1. The list contains 101 primes
-- 2. The sum of the primes is prime
-- 3. The list of primes is consecutive

prop_length101 :: [Integer] -> Bool
prop_length101 xs = 101 == length xs

prop_sumOfNConsecutivePrimesIsPrime :: Integer -> Bool
prop_sumOfNConsecutivePrimesIsPrime firstPrime = prime $ sum (take 101 $ primesFromN firstPrime)

prop_listOfPrimesIsConsecutive :: [Integer] -> Bool
prop_listOfPrimesIsConsecutive xs = xs == take 101 (primesFromN (head xs))

prop_combinedProps :: Bool
prop_combinedProps =
  let (firstPrime, _, listOfPrimes) = consecutive101Prime' primes
   in prop_length101 listOfPrimes && prime (sum listOfPrimes)

main :: IO ()
main = do
  let (firstPrime, sumPrime, primeList) = consecutive101Prime' primes
  quickCheck $ prop_sumOfNConsecutivePrimesIsPrime firstPrime
  quickCheck $ prop_length101 primeList
  quickCheck $ prop_listOfPrimesIsConsecutive primeList

-- Time spent: 1 hour
module Exercise6 where

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

-- A version that uses primes and skips all numbers before n
primesFromN :: Integer -> [Integer]
primesFromN n
  | n > 1 = filter prime [n ..]
  | otherwise = error "n needs to be bigger than 1"

-- You have to test your answer is correct with the following properties:
-- 1. The list contains 101 primes
-- 2. The sum of the primes is prime
-- 3. The list of primes is consecutive

consecutive101Prime' :: [Integer] -> (Integer, Integer, [Integer])
consecutive101Prime' remainingPrimes@(x : xs)
  | prime sum101Primes = (x, sum101Primes, listOf101Primes)
  | otherwise = consecutive101Prime' xs
  where
    listOf101Primes = take 101 remainingPrimes
    sum101Primes = sum listOf101Primes

consecutive101Prime :: Integer
consecutive101Prime = case consecutive101Prime' primes of
  (_, sumOf101Primes, _) -> sumOf101Primes

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

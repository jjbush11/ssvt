module Exercise8 where

import Test.QuickCheck

-- Was provided in Lab0
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

-- Was provided in Lab0
primes :: [Integer]
primes = 2 : filter isPrime [3 ..]

-- The function makes use of Haskell's lazy evaluation to create an infinite long list
-- One is able to get e.g. the first 10 counterexamples by using take 10 counterExamples
counterExamples :: [([Integer], Integer)]
counterExamples = counterExamplesFrom 2

-- A recursive function calculate the product of consecutive primes
-- If the product of those primes + 1 is not prime, it should be added to the list.
-- Because it keeps on adding counterexamples, the list can become infinitely long
counterExamplesFrom :: Int -> [([Integer], Integer)]
counterExamplesFrom n
  | not $ isPrime productPlusOne = (primeList, productPlusOne) : counterExamplesFrom (n + 1)
  | otherwise = counterExamplesFrom (n + 1)
  where
    primeList = take n primes
    productPlusOne = product primeList + 1

-- Exercise 8
main :: IO ()
main = do
  print $ take 10 counterExamples

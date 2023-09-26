module Exercise4 where

import Data.Foldable
import Data.Maybe
import GHC.Unicode (wgencat)
import System.Random
import Test.QuickCheck
import Data.List

-- From Exercise 1
-- A generator which only allows for natural numbers to be generates
genPositiveNumbers :: Gen Integer
genPositiveNumbers = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

-- A generator which generates numbers smaller or equal to zero
genNegativeNumbers :: Gen Integer
genNegativeNumbers = arbitrary `suchThat` (<= 0)

-- From https://prime-numbers.info/list/emirps-up-to-10000
emirps :: [Integer]
emirps = [13, 17, 31, 37, 71, 73, 79, 97, 107, 113, 149, 157, 167, 179, 199, 311, 337, 347, 359, 389, 701, 709, 733, 739, 743, 751, 761, 769, 907, 937, 941, 953, 967, 971, 983, 991, 1009, 1021, 1031, 1033, 1061, 1069, 1091, 1097, 1103, 1109, 1151, 1153, 1181, 1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1279, 1283, 1301, 1321, 1381, 1399, 1409, 1429, 1439, 1453, 1471, 1487, 1499, 1511, 1523, 1559, 1583, 1597, 1601, 1619, 1657, 1669, 1723, 1733, 1741, 1753, 1789, 1811, 1831, 1847, 1867, 1879, 1901, 1913, 1933, 1949, 1979, 3011, 3019, 3023, 3049, 3067, 3083, 3089, 3109, 3121, 3163, 3169, 3191, 3203, 3221, 3251, 3257, 3271, 3299, 3301, 3319, 3343, 3347, 3359, 3371, 3373, 3389, 3391, 3407, 3433, 3463, 3467, 3469, 3511, 3527, 3541, 3571, 3583, 3613, 3643, 3697, 3719, 3733, 3767, 3803, 3821, 3851, 3853, 3889, 3911, 3917, 3929, 7027, 7043, 7057, 7121, 7177, 7187, 7193, 7207, 7219, 7229, 7253, 7297, 7321, 7349, 7433, 7457, 7459, 7481, 7507, 7523, 7529, 7547, 7561, 7577, 7589, 7603, 7643, 7649, 7673, 7681, 7687, 7699, 7717, 7757, 7817, 7841, 7867, 7879, 7901, 7927, 7949, 7951, 7963, 9001, 9011, 9013, 9029, 9041, 9103, 9127, 9133, 9161, 9173, 9209, 9221, 9227, 9241, 9257, 9293, 9341, 9349, 9403, 9421, 9437, 9439, 9467, 9479, 9491, 9497, 9521, 9533, 9547, 9551, 9601, 9613, 9643, 9661, 9679, 9721, 9749, 9769, 9781, 9787, 9791, 9803, 9833, 9857, 9871, 9883, 9923, 9931, 9941, 9967]

-- From https://prime-numbers.info/list/palindromic-primes
palindromicPrimes :: [Integer]
palindromicPrimes = [2, 3, 5, 7, 11, 101, 131, 151, 181, 191, 313, 353, 373, 383, 727, 757, 787, 797, 919, 929]

reversibleStreamPreCalculated :: [Integer]
reversibleStreamPreCalculated = sort $ emirps ++ palindromicPrimes

prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where
    xs = takeWhile (\y -> y ^ 2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3 ..]

reversal :: Integer -> Integer
reversal = read . reverse . show

isReversablePrime :: Integer -> Bool
isReversablePrime n = prime (reversal n)

reversibleStream :: [Integer]
reversibleStream = filter isReversablePrime $ takeWhile (< 10000) primes

-- 1. Reversal correctness checks if the reversal of the reversal produces the original number
-- This property should fail for values below 0, e.g. -1 => 1-. This is because read can't parse 1- to an integer.
-- Furthermore, every number that ends with a 0 will be reversed wrongly. E.g. 10 => 01, which read parses to 1.
-- Thus this property will also fail for 10, 20, etc.
-- To fix this, the reversal function could contain a guard which checks if the amount of digits are the same at the end.
prop_reversalCorrectness :: Integer -> Bool
prop_reversalCorrectness n = reversal (reversal n) == n

-- 2. Prime reversibility
-- Tests the reversiblePrime function on a list found on prime-numbers.info
-- This ensures that the function actually works, because this list was predefined
prop_primesAreReversable :: Bool
prop_primesAreReversable = all isReversablePrime (emirps ++ palindromicPrimes)

-- 3. Prime Membership
-- This checks if all values in reversibleStream are prime.
-- The prime function was supplied in the Lab0.hs file
-- We expect that all primes
prop_primeMembership :: Bool
prop_primeMembership = all prime reversibleStream

-- 4. Reversible Prime Count
-- We expect the list to be a count of 260, because it contains both emirps (e.g. 13 <--> 31)
-- and palindromes (e.g. 11 <--> 11).
prop_reversiblePrimeCount :: Bool
prop_reversiblePrimeCount = length reversibleStreamPreCalculated == length reversibleStream

-- 5. Reversal Symmetry
-- We want to verify for every element in the list also has an element in the list that is in reverse way.
-- First we make a function to find an element in the list that is equal to the reversal prime
findReversible :: Integer -> Maybe Integer
findReversible n = find precondition reversibleStream
  where
    precondition x = x == reversal n

-- We map above mentioned function, findReversible, which returns a list of [Maybe Integer]
-- We check if list doesn't contain Nothing. If so the test succeeds.
prop_reversalSymmetry :: Bool
prop_reversalSymmetry = Nothing `notElem` map findReversible reversibleStream

-- 6. Unique Values
-- First create a function to check if a list only contains unique values
uniqueValues :: Eq a => [a] -> Bool
uniqueValues [] = True
uniqueValues (x : xs) = x `notElem` xs && uniqueValues xs

-- Testing if above function returns true when unique list of Integers is provided
prop_uniqueValuesTrue :: Bool
prop_uniqueValuesTrue = uniqueValues [1, 2, 3]

-- Testing if above function returns fails when a non unique list of Integers is provided
prop_uniqueValuesFalse :: Bool
prop_uniqueValuesFalse = not $ uniqueValues [2, 3, 3]

-- Now check if reversibleStream only contains unique values
prop_uniqueValue :: Bool
prop_uniqueValue = uniqueValues reversibleStream

-- 7. Check Maximum Value
-- Looks up the maximum value in the list and checns if it is below or equal to 10000
prop_maxValue :: Bool
prop_maxValue = maximum reversibleStream <= 10000

main :: IO ()
main = do
  -- Test the properties
  -- Test both properties with the genPositiveNumbers generator and the genNegativeNumbers generator
  -- Expect failure on negative numbers
  quickCheck $ forAll genPositiveNumbers prop_reversalCorrectness
  quickCheck $ expectFailure $ forAll genNegativeNumbers prop_reversalCorrectness
  quickCheck prop_primesAreReversable
  quickCheck prop_primeMembership
  quickCheck prop_reversiblePrimeCount
  quickCheck prop_reversalSymmetry
  quickCheck prop_uniqueValuesTrue
  quickCheck prop_uniqueValuesFalse
  quickCheck prop_uniqueValue
  quickCheck prop_maxValue

-- Time spent: 1.5 hours
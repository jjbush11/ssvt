module Exercise2 where

import Data.Char
import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n - 1)
  return (p : ps)

-- Exercise 2
-- We have to use monadicIO, because probs has side effects as it uses getStdRandom.
-- The property will then pass the generated probabilities to checkIfDistributionIsEqual
prop_quartileDistrubution :: Int -> Property
prop_quartileDistrubution n = monadicIO $ do
  values <- run (probs n)
  return $ checkIfDistributionIsEqual values

checkIfDistributionIsEqual :: [Float] -> Bool
checkIfDistributionIsEqual floatList
  | all (checkIfNumberWithInMargin quartile errorMargin) [firstRangeCount, secondRangeCount, thirdRangeCount, fourthRangeCount] = True
  | otherwise = False
  where
    quartile = length floatList `div` 4
    -- The error margin is 0.05
    errorMargin = length floatList `div` 20
    firstRangeCount = getNumberOfItemsInRange floatList 0 0.25
    secondRangeCount = getNumberOfItemsInRange floatList 0.25 0.5
    thirdRangeCount = getNumberOfItemsInRange floatList 0.5 0.75
    fourthRangeCount = getNumberOfItemsInRange floatList 0.75 1

-- This function is to count all numbers that are in a specific range
getNumberOfItemsInRange :: [Float] -> Float -> Float -> Int
getNumberOfItemsInRange floatList lowerBound upperBound = length (filter (\x -> (x > lowerBound) && (x <= upperBound)) floatList)

-- This function check if actuel quartile size is within expected size and margin error.
checkIfNumberWithInMargin :: Int -> Int -> Int -> Bool
checkIfNumberWithInMargin expectedQuartileSize errorMargin actualQuartileSize = actualQuartileSize >= expectedQuartileSize - errorMargin && actualQuartileSize <= expectedQuartileSize + errorMargin

-- A list of numbers with an unequal distribution
unequalDistribution :: [Float]
unequalDistribution = [0.2, 0.2, 0.2, 0.75]

main :: IO ()
main = do
  -- Test for multiple float list sizes. small size lists tends to have a non equal distrubution.
  -- Thus the lists of counts 100 and 1000 fail
  quickCheckWith stdArgs {maxSuccess = 1000} (prop_quartileDistrubution 100)
  quickCheckWith stdArgs {maxSuccess = 1000} (prop_quartileDistrubution 1000)
  quickCheckWith stdArgs {maxSuccess = 1000} (prop_quartileDistrubution 10000)

  -- Expect a failure due to an unequal distribution
  quickCheck $ expectFailure $ checkIfDistributionIsEqual unequalDistribution

-- Time spend: 1.5 hour
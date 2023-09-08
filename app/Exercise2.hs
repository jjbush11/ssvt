module Exercise2 where

import Control.Monad
import Data.Char
import Data.List
import GHC.Float
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.State (State (expected))

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
  p <- getStdRandom random
  ps <- probs (n - 1)
  return (p : ps)

-- Exercise 2
-- Quartile boundaries
quartileBoundaries :: [Float]
quartileBoundaries = [0, 0.25, 0.5, 0.75, 1]

-- Expected proportions for a fair die
expectedProportions :: Int -> [Float]
expectedProportions n = replicate 4 (fromIntegral (n `div` 4))

-- Allowed tolerance for each proportion
tolerance :: Double
tolerance = 0.05

-- Helper function to count the number of values within a quartile boundary
countValuesInQuartile :: [Float] -> Float -> Float -> Int
countValuesInQuartile values lowerBound upperBound =
  length $ filter (\v -> v >= lowerBound && v < upperBound) values

-- Helper function to calculate the expected counts for each quartile
calculateExpectedCounts :: Int -> [Float]
calculateExpectedCounts n =
  replicate 4 (fromIntegral (n `div` 4))

-- Helper function to check if observed counts are within tolerance of expected counts
withinTolerance :: [(Int, Float)] -> Float -> Bool
withinTolerance counts tolerance =
  all (\(observed, expected) -> abs (fromIntegral observed - expected) <= tolerance) counts

prop_QuartileDistribution :: Int -> Property
prop_QuartileDistribution n = monadicIO $ do
  values <- run (probs n)

  -- Define quartile boundaries
  let quartileBoundaries = [0.0, 0.25, 0.5, 0.75, 1.0]

  -- Calculate observed counts for each quartile
  let quartileCounts =
        [ (countValuesInQuartile values q (q + 0.25), expected)
          | (q, expected) <- zip quartileBoundaries (calculateExpectedCounts n)
        ]

  -- Define tolerance
  let tolerance = 0.05 * fromIntegral n :: Float

  -- Assert that observed counts are within tolerance of expected counts
  assert (withinTolerance quartileCounts tolerance)

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} (prop_QuartileDistribution 10000)

-- Time spent: 2 hour
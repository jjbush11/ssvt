module Lab1.Exercise1 where

import Control.Monad
import Data.Char
import Data.List
import GHC.Float
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.State

-- Exercise 1

test :: Integer -> Bool
test n = n >= 0


main :: IO ()
main = do
  quickCheck test

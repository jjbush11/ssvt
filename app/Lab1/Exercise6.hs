module Lab1.Exercise6 where

import Control.Monad
import Data.Char
import Data.List
import GHC.Float
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.State

-- Exercise 6
-- Converting from first-order logic

-- Convert to negation normal form 
-- Eliminate implications
-- Repeatedly replace P -> Q with ~P v Q

-- Replace P <-> Q with (P v ~Q) ^ (~P v Q)

main :: IO ()
main = do
  print "Hello World"

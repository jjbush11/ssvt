module Main where
import Test.QuickCheck
import Control.Monad
import Lab0.TestSuite
import Lab1.TestSuite

main :: IO ()
main = do 
    -- Lab 0 Exercises
    Lab0.TestSuite.main
    -- Lab 1 Exercises
    Lab1.TestSuite.main

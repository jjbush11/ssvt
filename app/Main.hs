module Main where
import Test.QuickCheck
import Control.Monad
import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Exercise5
import Exercise6


main :: IO ()
main = do 
    -- Exercise 1
    putStrLn "\n**Performing tests for exercise 1"
    Exercise1.main
    -- Exercise 2
    putStrLn "\n**Performing tests for exercise 2"
    Exercise2.main
    -- Exercise 3
    putStrLn "\n**Performing tests for exercise 3"
    Exercise3.main
    -- Exercise 4 
    putStrLn "\n**Performing tests for exercise 4"
    Exercise4.main
    -- Exercise 5
    putStrLn "\n**Performing tests for exercise 5"
    Exercise5.main
    -- Exercise 6
    putStrLn "\n**Performing tests for exercise 6"
    Exercise6.main
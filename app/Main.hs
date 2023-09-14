module Main where
import Test.QuickCheck
import Control.Monad
import Lab0.Exercise1
import Lab0.Exercise2
import Lab0.Exercise3
import Lab0.Exercise4
import Lab0.Exercise5
import Lab0.Exercise6
import Lab1.Exercise1


main :: IO ()
main = do 
    -- Lab 0 Exercise 1
    putStrLn "\n**Performing tests for lab 0 exercise 1"
    Lab0.Exercise1.main
    -- Lab 0 Exercise 2
    putStrLn "\n**Performing tests for lab 0 exercise 2"
    Lab0.Exercise2.main
    -- Lab 0 Exercise 3
    putStrLn "\n**Performing tests for lab 0 exercise 3"
    Lab0.Exercise3.main
    -- Lab 0 Exercise 4 
    putStrLn "\n**Performing tests for lab 0 exercise 4"
    Lab0.Exercise4.main
    -- Lab 0 Exercise 5
    putStrLn "\n**Performing tests for lab 0 exercise 5"
    Lab0.Exercise5.main
    -- Lab 0 Exercise 6
    putStrLn "\n**Performing tests for lab 0 exercise 6"
    Lab0.Exercise6.main    
    -- Lab 1 Exercise 1
    putStrLn "\n**Performing tests for lab 1 exercise 1"
    Lab1.Exercise1.main
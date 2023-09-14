module Lab1.TestSuite where

import Lab1.Exercise1
import Lab1.Exercise2
import Lab1.Exercise3
import Lab1.Exercise4
import Lab1.Exercise5
import Lab1.Exercise6
import Lab1.Exercise7
import Lab1.Exercise8

main :: IO ()
main = do 
    -- Lab 1 Exercise 1
    putStrLn "\n** Performing tests for lab 1 exercise 1"
    Lab1.Exercise1.main
    -- Lab 1 Exercise 2
    putStrLn "\n** Performing tests for lab 1 exercise 2"
    Lab1.Exercise2.main
    -- Lab 1 Exercise 3
    putStrLn "\n** Performing tests for lab 1 exercise 3"
    Lab1.Exercise3.main
    -- Lab 1 Exercise 4 
    putStrLn "\n** Performing tests for lab 1 exercise 4"
    Lab1.Exercise4.main
    -- Lab 1 Exercise 5
    putStrLn "\n** Performing tests for lab 1 exercise 5"
    Lab1.Exercise5.main
    -- Lab 1 Exercise 6
    putStrLn "\n** Performing tests for lab 1 exercise 6"
    Lab1.Exercise6.main    
    -- Lab 1 Exercise 7
    putStrLn "\n** Performing tests for lab 1 exercise 7"
    Lab1.Exercise7.main    
    -- Lab 1 Exercise 8
    putStrLn "\n** Performing tests for lab 1 exercise 8"
    Lab1.Exercise8.main    
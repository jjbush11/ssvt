module Main where

import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Exercise5
import Exercise6
import Exercise7
import Exercise8
import Exercise9

lab :: Int
lab = undefined

printTesting :: Int -> IO ()
printTesting exercise = putStrLn $ "\n** Performing tests for Lab " ++ show lab ++ " Exercise " ++ show exercise

main :: IO ()
main = do
  -- Lab 1 Exercise 1
  printTesting 1
  Exercise1.main
  -- Lab 1 Exercise 2
  printTesting 2
  Exercise2.main
  -- Lab 1 Exercise 3
  printTesting 3
  Exercise3.main
  -- Lab 1 Exercise 4
  printTesting 4
  Exercise4.main
  -- Lab 1 Exercise 5
  printTesting 5
  Exercise5.main
  -- Lab 1 Exercise 6
  printTesting 6
  Exercise6.main
  -- Lab 1 Exercise 7
  printTesting 7
  Exercise7.main
  -- Lab 1 Exercise 8
  printTesting 8
  Exercise8.main
  -- Lab 1 Exercise 9
  printTesting 9
  Exercise9.main
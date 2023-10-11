module Main where

import Exercise1
import Exercise2
import Exercise3
import Exercise4
import Exercise5
import Exercise6
import Exercise7
import Bonus1

lab :: Int
lab = 4

printTesting :: Int -> IO ()
printTesting exercise = putStrLn $ "\n** Performing tests for Lab " ++ show lab ++ " Exercise " ++ show exercise

main :: IO ()
main = do
  -- Lab 4 Exercise 1
  printTesting 1
  Exercise1.main
  -- Lab 4 Exercise 2
  printTesting 2
  Exercise2.main
  -- Lab 4 Exercise 3
  printTesting 3
  Exercise3.main
  -- Lab 4 Exercise 4
  printTesting 4
  Exercise4.main
  -- Lab 4 Exercise 5
  printTesting 5
  Exercise5.main
  -- Lab 4 Exercise 6
  printTesting 6
  Exercise6.main
  -- Lab 4 Exercise 7
  printTesting 7
  Exercise7.main
  -- Lab 4 Bonus Exercise 1
  putStrLn $ "\n** Performing tests for Lab " ++ show lab ++ " Bonus Exercise 1"
  Bonus1.main

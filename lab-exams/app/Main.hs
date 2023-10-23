module Main where

import Exam01022023

lab :: Int
lab = 0

printTesting :: Int -> IO ()
printTesting exercise = putStrLn $ "\n** Performing tests for Lab " ++ show lab ++ " Exercise " ++ show exercise

main :: IO ()
main = do
    Exam01022023.main
  
module Main where

import Exam01022023
import Exam25102023Max1
import Exam25102023Max2
import Exam25102023Olaf
import Exam27102021

lab :: Int
lab = 0

printTesting :: Int -> IO ()
printTesting exercise = putStrLn $ "\n** Performing tests for Lab " ++ show lab ++ " Exercise " ++ show exercise

main :: IO ()
main = do
    -- Exam25102023Max1.main
    Exam25102023Max2.main  
    -- Exam27102021.main
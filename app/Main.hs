module Main where
import Lab0
import Test.QuickCheck (quickCheck)


main :: IO ()
main = do 
    quickCheck prop_squareSum
    quickCheck prop_powerOfThreeSum
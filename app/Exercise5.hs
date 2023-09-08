module Exercise5 where

import Data.Char

charToInt :: Char -> Int
charToInt c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'
  | otherwise = 0

intToChar :: Int -> Char
intToChar n
  | 0 <= n && n < 26 = chr $ n + ord 'a'
  | n >= 26 && n < 52 = chr $ n + ord 'A'
  | otherwise = ' '

rot13 :: [Char] -> [Char]
rot13 = map rot13'
  where
    rot13' :: Char -> Char
    rot13' c
      | isAlpha c = intToChar $ (charToInt c + 13) `mod` 26
      | otherwise = c

main :: IO ()
main = do
  print $ rot13 "Smurfen"
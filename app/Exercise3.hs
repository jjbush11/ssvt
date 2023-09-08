module Exercise3 where

import System.Random
import Test.QuickCheck
import GHC.Read (list)

data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)

isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = a + b <= c || b + c <= a || c + a <= b

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = all (== a) [b, c]

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = a ^ 2 + b ^ 2 == c ^ 2 || b ^ 2 + c ^ 2 == a ^ 2 || c ^ 2 + a ^ 2 == b ^ 2

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || b == c || c == a

-- Exercise 3
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  -- A triangle is invalis if the sum of the lengths of any two sides is <= the third side.
  | isNoTriangle a b c = NoTriangle
  -- All three sides are of equal length
  | isEquilateral a b c = Equilateral
  -- One angle is a right angle, which means it should follow the Pythagoras theorem
  | isRectangular a b c = Rectangular
  -- At least two sides are of equal length
  | isIsosceles a b c = Isosceles
  -- A triangle which doesn't fall into the above categories
  | otherwise = Other

-- Uses Euclid's formula to generate pythagorean triples
-- https://www.wikiwand.com/en/Pythagorean_triple
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples =
  [ (a, b, c) | m <- [2 ..], n <- [1 .. (m - 1)], let a = m ^ 2 - n ^ 2, let b = 2 * m * n, let c = m ^ 2 + n ^ 2, a ^ 2 + b ^ 2 == c ^ 2
  ]

generateTwoSides :: RandomGen g => g -> (Integer, Integer)
generateTwoSides g = (a, b)
  where
    [a, b] = take 2 $ randomRs (1, 100) g

generateTriangle :: RandomGen g => g -> Shape -> (Integer, Integer, Integer)
generateTriangle g NoTriangle = (a, b, c)
  where
    (a, b) = generateTwoSides g
    c = a + b + 1
generateTriangle g Equilateral = (a, a, a)
  where
    (a, _) = randomR (1, 100) g
generateTriangle g Isosceles = (a, a, c)
  where
    (a, b) = generateTwoSides g
    c = min a b + 1
generateTriangle g Rectangular = (a, b, c)
  where
    (a, b, c) = pythagoreanTriples !! fst (randomR (0, 100) g)
generateTriangle g Other = (a, b, c)
  where
    (a, b) = generateTwoSides g
    c = a + b - 1

prop_checkTriangle :: RandomGen g => g -> Shape -> Bool
prop_checkTriangle g shape = triangle a b c == shape
  where
    (a, b, c) = generateTriangle g shape

main :: IO ()
main = do
  -- Define a random generator
  g <- newStdGen
  -- Generate a list of 100 random triangle shapes (NoTriangle, Equilateral, Isosceles, Rectangular, Other)
  -- The list should look like [NoTriangle, Equilateral, Isosceles, ...]
  listOfShapes <- generate $ vectorOf 100 $ elements [NoTriangle, Equilateral, Isosceles, Rectangular, Other]
  -- generate 100 triangles based on the list of shapes
  let listOfTriangles = map (generateTriangle g) listOfShapes
  -- determine the shape of each triangle
  let results = map (\(a, b, c) -> triangle a b c) listOfTriangles
  -- Compare the listOfShapes with the results and see if they are equal
  putStrLn $ "listOfShapes == results: " ++ show (listOfShapes == results)
  -- A test in QuickCheck which tests if the triangle function is correct
  -- It does this by generating a random triangle and comparing the result of the triangle function with the expected result


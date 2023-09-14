{-# LANGUAGE InstanceSigs #-}

module Lab0.Exercise3 where

import GHC.Read (list)
import qualified GHC.TypeLits as RandomGen
import System.Random
import Test.QuickCheck

data Shape
  = NoTriangle
  | Equilateral
  | Isosceles
  | Rectangular
  | Other
  deriving (Eq, Show)

-- An Arbitrary instance for Shape
instance Arbitrary Shape where
  arbitrary :: Gen Shape
  arbitrary =
    oneof
      [ return NoTriangle,
        return Equilateral,
        return Isosceles,
        return Rectangular,
        return Other
      ]

-- A triangle is invalid if the sum of the lengths of any two sides is <= the third side.
isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = a + b <= c || b + c <= a || c + a <= b

-- All three sides are of equal length
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = all (== a) [b, c]

-- One angle is a right angle, which means it should follow the Pythagoras theorem
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = a ^ 2 + b ^ 2 == c ^ 2 || b ^ 2 + c ^ 2 == a ^ 2 || c ^ 2 + a ^ 2 == b ^ 2

-- At least two sides are of equal length
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a == b || b == c || c == a

-- Returns which type of triangle coincides with the three sides provided
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | isNoTriangle a b c = NoTriangle
  | isEquilateral a b c = Equilateral
  | isRectangular a b c = Rectangular
  | isIsosceles a b c = Isosceles
  -- A triangle which doesn't fall into the above categories
  | otherwise = Other

-- Uses Euclid's formula to generate pythagorean triples, this is needed to create rectangular triangles.
-- This is needed due to our triangle function having integers as input instead of floats.
-- https://www.wikiwand.com/en/Pythagorean_triple
pythagoreanTriples :: [(Integer, Integer, Integer)]
pythagoreanTriples =
  [(a, b, c) | m <- [2 ..], n <- [1 .. (m - 1)], let a = m ^ 2 - n ^ 2, let b = 2 * m * n, let c = m ^ 2 + n ^ 2, a ^ 2 + b ^ 2 == c ^ 2]

-- Generates two random side lengths between 1 and 100
generateTwoSides :: RandomGen g => g -> (Integer, Integer)
generateTwoSides g = (a, b)
  where
    [a, b] = take 2 $ randomRs (1, 100) g

-- We tried to keep the function pure, so the function needs to be provided with a generator
generateTriangle :: RandomGen g => g -> Shape -> (Integer, Integer, Integer)
-- If a + b < c, then the triangle is invalid
generateTriangle g NoTriangle = (a, b, c)
  where
    (a, b) = generateTwoSides g
    c = a + b + 1
-- All sides are equal
generateTriangle g Equilateral = (a, a, a)
  where
    (a, _) = randomR (1, 100) g
-- Two sides are equal
generateTriangle g Isosceles = (a, a, c)
  where
    (a, b) = generateTwoSides g
    c = min a b + 1
-- Takes a random pythagoreanTriple
generateTriangle g Rectangular = (a, b, c)
  where
    (a, b, c) = pythagoreanTriples !! fst (randomR (0, 100) g)
-- A triangle which is in the Other category
generateTriangle g Other = (a, b, c)
  where
    (a, b) = generateTwoSides g
    c = a + b - 1

-- A property we wanted to test with QuickCheck, but we weren't able to make it generate a RandomGen.
-- An instance of Arbitrary for Shape is at the start of the code
-- What it should do is generate a triangle based on a random shape and check if triangle categorizes it correctly
prop_checkTriangle :: RandomGen g => g -> Shape -> Bool
prop_checkTriangle g shape = triangle a b c == shape
  where
    (a, b, c) = generateTriangle g shape

main :: IO ()
main = do
  -- Define a random generator
  -- Unfortunately, we didn't manage to make QuickCheck generate more generators
  -- So the first side of every triangle is the same.
  g <- newStdGen
  -- Generate a list of 100 random triangle shapes (NoTriangle, Equilateral, Isosceles, Rectangular, Other)
  -- The list should look like [NoTriangle, Equilateral, Isosceles, ...]
  listOfShapes <- generate $ vectorOf 100 $ elements [NoTriangle, Equilateral, Isosceles, Rectangular, Other]
  -- Generate 100 triangles based on the list of shapes
  let listOfTriangles = map (generateTriangle g) listOfShapes
  -- Determine the shape of each triangle
  let results = map (\(a, b, c) -> triangle a b c) listOfTriangles
  -- Test if the shapes detected by triangle are the same as the ones in listOfShapes
  quickCheck $ listOfShapes == results

-- Time spent: 2 hour
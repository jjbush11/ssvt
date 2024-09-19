module Exam25102023Olaf where

-- import Test.FitSpec
import Data.List
import Exercise3 (straces)
import Exercise4 (after)
import LTS (IOLTS)
import Test.QuickCheck

-- == Problem 2 ==

type Rel a = [(a, a)]

domain :: (Eq a) => Rel a -> [a]
domain r = (map fst r) `union` (map snd r)

composition :: (Ord a) => Rel a -> Rel a -> Rel a
composition r s = [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

deltaA :: (Eq a) => [a] -> Rel a
deltaA xs = [(x, x) | x <- xs]

irreflexive :: (Eq a) => Rel a -> Bool
irreflexive r = not $ any (\x -> (x, x) `elem` r) (domain r)

coreflexive :: (Eq a) => Rel a -> Bool
coreflexive = all (\(x, y) -> x == y)

transitive :: (Eq a) => Rel a -> Bool
transitive r = and [elem (x, z) r | (x, y) <- r, (y', z) <- r, y == y']

antisymmetric :: (Eq a) => Rel a -> Bool
antisymmetric r = and [x == y | (x, y) <- r, (y, x) `elem` r]

asymmetric :: (Eq a) => Rel a -> Bool
asymmetric r = irreflexive r && antisymmetric r

inverse :: (Eq a) => Rel a -> Rel a
inverse r = [(y, x) | (x, y) <- r]

symClos :: (Ord a) => Rel a -> Rel a
symClos r = sort $ nub $ r ++ inverse r

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (\x -> elem x ys) xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

-- To test: quickCheck $ forAll (arbitrary `suchThat` (> 0)) prop_notComparable
prop_notComparable :: Int -> Bool
prop_notComparable n = not (subset lhs rhs) && not (subset rhs lhs)
  where
    a = [1 .. n]
    rels = subsets $ sort $ nub [(x, y) | x <- a, y <- a]
    lhs = filter (\r -> coreflexive r && transitive r) rels
    rhs = filter (\r -> asymmetric r && transitive r) rels

-- == Problem 3 ==
numNRIr :: Int -> Int
numNRIr 0 = 0
numNRIr 1 = 0
numNRIr n = 2 ^ (n ^ 2 - 1) + 2 ^ (2 * n - 2) * numNRIr (n - 1)

-- quickCheck $ forAll (arbitrary `suchThat` (>= 0)) prop_expectedOutput
prop_expectedOutput :: Int -> Bool
prop_expectedOutput n
  | n < 2 = numNRIr n == 0
  | otherwise = numNRIr n == (2 ^ n - 2) * (2 ^ (n ^ 2 - n))

-- quickCheck $ forAll (arbitrary `suchThat` (>= 0)) prop_nonNegative
prop_nonNegative :: Int -> Bool
prop_nonNegative n = numNRIr n >= 0

-- == Problem 4 ==
type State = Integer

type Label = String

type LabeledTransition = (State, Label, State)

type LTS = ([State], [Label], [LabeledTransition], State)

tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper

delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

dispenserImpl :: LTS
dispenserImpl = ([1 .. 4], ["?btn", "?switch", "!water"], [(1, "?btn", 2), (1, "?switch", 1), (1, "?btn", 3), (3, "!water", 4)], 1)

dispenserModel :: LTS
dispenserModel = ([1 .. 4], ["?btn", "!water"], [(1, "?btn", 2), (2, "tau", 3), (2, "!water", 4)], 1)

ltsToIolts :: LTS -> IOLTS
ltsToIolts (states, labels, transitions, initial) = (states, labelIn, labelOut, transitions, initial)
  where
    labelIn = filter (\l -> head l == '?') labels
    labelOut = filter (\l -> head l == '!') labels

-- Given a trace, determine whether water is dispensed by the model
superWater :: [Label] -> Bool
superWater trace = after (ltsToIolts dispenserImpl) trace == [4]

out :: IOLTS -> [State] -> [Label]
out (_, _, _, transitions, _) states = map (\(_, label, _) -> label) $ filter (\(s, l, _) -> (s `elem` states) && (head l == '!')) transitions

order :: LTS
order =
  ( [0 .. 5],
    ["?coin", "?btn", "!food", "!drink", "!receipt"],
    [ (0, "?coin", 1),
      (1, "?coin", 2),
      (2, "!food", 3),
      (0, "?btn", 3),
      (3, "!receipt", 4),
      (0, "?coin", 5),
      (5, "!drink", 3)
    ],
    0
  )

-- == Problem 5 ==
corona r s x0 t = r ^ t * x0 + s * (r ^ t - 1) / (r - 1)

genCorona :: Gen (Double, Double, Double, Int)
genCorona = do
  r <- arbitrary `suchThat` (> 1)
  s <- arbitrary `suchThat` (> 0)
  x0 <- arbitrary `suchThat` (> 0)
  t <- arbitrary `suchThat` (> 0)
  return (r, s, x0, t)

-- quickCheck $ forAll genCorona prop_coronaPositive
prop_coronaPositive :: (Double, Double, Double, Int) -> Bool
prop_coronaPositive (r, s, x0, t) = corona r s x0 t > 0

-- quickCheck $ forAll genCorona prop_coronaIncreasing
prop_coronaIncreasing :: (Double, Double, Double, Int) -> Bool
prop_coronaIncreasing (r, s, x0, t) = corona r s x0 (t + 1) > corona r s x0 t

-- quickCheck $ forAll genCorona prop_coronaMedical
prop_coronaMedical :: Double -> Int -> Property
prop_coronaMedical s t = s > 0 && t > 0 ==> corona 0 s 0 t == s

-- Test using fitSpec (sadly did not work.)
-- properties multiplicationTable =
--   [ Test.FitSpec.property $ prop_coronaCorrect
--   , Test.FitSpec.property $ prop_coronaPositive
--   , Test.FitSpec.property $ prop_coronaIncreasing
-- --   , Test.FitSpec.property $ prop_coronaMedical
--   ]

-- testFitSpec = mainWith args { names = ["multiplicationTable x"]
--                      , nMutants = 400
--                      , nTests = 400
--                      , timeout = 0
--                      }
--                 (corona :: Double -> Double -> Double -> Int -> Double)
--                 properties

main :: IO ()
main = do
  -- Problem 3
  -- Test the superWater function
  print "The following two values should hold True"
  print $ superWater ["?btn", "!water"]
  print $ superWater ["?switch", "?btn", "!water"]

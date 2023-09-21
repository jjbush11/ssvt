module Lab1.Exercise6 where

import Test.QuickCheck
import Lecture3

-- Exercise 6
-- Converting from first-order logic

-- Example 1: Negation of Disjunction
example1 :: Form
example1 = Neg (Dsj [Prop 1, Prop 2])

-- Example 2: Implication
example2 :: Form
example2 = Impl (Prop 1) (Prop 2)

-- Example 3: Nested Equivalence
example3 :: Form
example3 = Equiv (Equiv (Prop 1) (Prop 2)) (Prop 3)

-- Example 4: Negation of a Complex Formula
example4 :: Form
example4 = Neg (Impl (Prop 1) (Cnj [Prop 2, Prop 3]))

-- Example 5: Combination of Connectives
example5 :: Form
example5 = Impl (Cnj [Prop 1, Dsj [Prop 2, Prop 3]]) (Prop 4)


-- Convert to negation normal form 
cnf :: Form -> Form
cnf = nnf . arrowfree


-- Eliminate implications
-- Repeatedly replace P -> Q with ~P v Q

-- Replace P <-> Q with (P v ~Q) ^ (~P v Q)

main :: IO ()
main = do
  putStrLn $ concatMap (\e -> show e ++ " \n" ) [example1, example2, example3, example4, example5]
  putStrLn $ concatMap (\e -> show (cnf e) ++ " \n" ) [example1, example2, example3, example4, example5]
  print $ cnf form2

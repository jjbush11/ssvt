module Exercise6 where

import Lecture3
import Test.QuickCheck

-- Exercise 6
-- Too showcase that our cnf converter works we created 5 examples.
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

-- Convert to Conjunctive normal form
-- The first step to convert a formula to Conjunctive normal form is removing it's arrows.
-- The following equivalences are used:
-- p→q becomes ¬p∨q
-- p↔q becomes (¬p∨q)∧(p∨¬q).
-- The arrowfree function from Lecture 3 provides this functionality.
-- When all the arrows are removed you need to make sure that only atoms can have negations.
-- To remove negations from non atoms the nnf function uses the following equivalences:
-- ¬¬p becomes p
-- ¬(p∧q) becomes ¬p∨¬q
-- ¬(p∨q) becomes ¬p∧¬q
-- Once again the functionality is provided by the nnf function from lecture 3.
cnf :: Form -> Form
cnf = nnf . arrowfree

main :: IO ()
main = do
  print "Examples:"
  putStrLn $ concatMap (\e -> show e ++ " \n") [example1, example2, example3, example4, example5]
  putStrLn $ concatMap (\e -> show (cnf e) ++ " \n") [example1, example2, example3, example4, example5]
  print "Examples from Lecture 3:"
  putStrLn $ concatMap (\e -> show e ++ " \n") [form1, form2, form3]
  putStrLn $ concatMap (\e -> show (cnf e) ++ " \n") [form1, form2, form3]
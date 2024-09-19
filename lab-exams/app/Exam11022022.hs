module Exam11022022
where

import Data.Foldable
import Test.QuickCheck
import Data.List


-- == Problem 2 ==

-- Relations ===============================================================
-- is reflexive
isReflexive :: [(Integer, Integer)] -> Bool
isReflexive xs = all (\x -> (x,x) `elem` xs) $ map fst xs

-- is coreflixive if xRy => x = y
isCoreflexive :: [(Integer, Integer)] -> Bool
isCoreflexive xs = all (\(x,y) -> x == y) xs
-- ===============================================================

-- Properties ===============================================================
corefProp1 :: [(Integer, Integer)] -> Bool
corefProp1 xs = isReflexive xs == isCoreflexive xs
-- ===============================================================

-- == Problem 3 ==
fib :: Int -> Int
fib n = f n 1 1 where
    f 0 n1 n2 = n1
    f n n1 n2 = f (n-1) n2 (n1+n2)

-- Properties ===============================================================
fibProp1 :: Int -> Property
fibProp1 n = n > 2 ==> fib n == fib (n - 1) + fib (n - 2)

fibProp2 :: Int -> Property
fibProp2 n = n > 1 ==> fib n > fib (n-1)

fibProp3 :: Int -> Property
fibProp3 n = n > 0 ==> fib n > 0
-- ===============================================================

-- == Problem 4 ==
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type LTS = ([State], [Label], [LabeledTransition], State)
-- -- IOLTS from lab 2
-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
tau = "tau" -- Please assume tau behaves as it is defined in the Tretmans paper
delta = "delta" -- Please assume delta behaves as it is defined in the Tretmans paper

fruitImpl :: LTS
fruitImpl = ([1..5], ["?btn", "!apple", "!kiwi"], [(1, "?btn", 2), (2, "!apple", 3), (1, "?btn", 4), (4, "!kiwi", 5)], 1)

fruitModel :: LTS
fruitModel = ([1..4], ["?btn", "!apple"], [(1, "?btn", 2), (2, tau, 3), (2, "!apple", 4)], 1)

-- -- Create IOLTS ===============================================================
-- -- Create IOLTS from LTS LabeledTransition from lab 2
-- -- Converts an LTS into an IOLTS
-- -- We do this because our after function from lab 2 works on IOLTS
-- createIOLTS :: [LabeledTransition] -> IOLTS
-- createIOLTS transitions = (states, map tail $ filter (\x -> head x == '?') labels, map tail $ filter (\x -> head x == '!') labels, map (\(f, l, t) -> (f, makeLabel l, t)) transitionSet, initState)
--   where
--     (states, labels, transitionSet, initState) = createLTS transitions

-- makeLabel :: Label -> Label
-- makeLabel x
--   | fst == '?' || fst == '!' = tail x
--   | otherwise = x
--   where
--     fst = head x

-- -- Creates an LTS from a list of transitions. Assumes that this list describes all states and labels, and that the lowest numbered state is the initial state.
-- createLTS :: [LabeledTransition] -> LTS
-- createLTS transitions = (states, filter (/= tau) $ makeSet (map (\(_, label, _) -> label) transitions), makeSet transitions, head states)
--   where
--     states = makeSet (concatMap (\(from, _, to) -> [from, to]) transitions)

-- makeSet :: (Ord a) => [a] -> [a]
-- makeSet = sort . nub
-- -- ===============================================================

-- -- After function from lab 2===============================================================
    
-- -- Function to find the target states of transitions with a given source state and label
-- findTransitions :: State -> Label -> [LabeledTransition] -> [State]
-- findTransitions s l transitions = [s'' | (s', l', s'') <- transitions, s == s', l == l']

-- -- Helper function to compute the set of states that can be reached from a given state and trace
-- -- It starts from the initial state given by the IOLTS
-- -- It then recursively calls itself with the next state and the remaining trace
-- -- All the results are concatenated and duplicates are removed
-- afterHelper :: IOLTS -> State -> [Label] -> [State]
-- afterHelper lts s [] = [s]
-- afterHelper lts@(states, inputLabels, outputLabels, transitions, initialState) s (l:ls) =
--   let nextStates = findTransitions s l transitions
--       afterNextState = concatMap (\s' -> afterHelper lts s' ls) nextStates
--   in nub afterNextState

-- -- The set of states that can be reached from a given state and trace
-- -- It starts from the initial state
-- -- This is passed to the helper function
-- after :: IOLTS -> [Label] -> [State]
-- after (_,_,_,_,initialState) [] = [initialState]
-- after lts@(_,_,_,_,initialState) trace = afterHelper lts initialState trace
-- ===============================================================

-- Properties ===============================================================
-- ltsToIolts :: LTS -> IOLTS
-- ltsToIolts lts@(_,_,labeledTransitions,_) = createIOLTS labeledTransitions

-- dispensesFruit :: [Label] -> Bool
-- dispensesFruit = dispensesFruit' (ltsToIolts fruitImpl)

-- dispensesFruit' :: IOLTS -> [Label] -> Bool
-- dispensesFruit' lts [] = False
-- dispensesFruit' lts trace = undefined

-- ===============================================================
dispensesFruit :: [Label] -> Bool
dispensesFruit [] = False
dispensesFruit (x : xs) = checkFruit 1 x xs

checkFruit :: State -> Label -> [Label] -> Bool
checkFruit currentState currentLabel [] = if (currentLabel == "!apple") || (currentLabel == "!kiwi") then True else False
checkFruit currentState currentLabel (nextLabel : rest)
  | (currentLabel == "!apple") || (currentLabel == "!kiwi") = True
  | otherwise = if nextState == -1 then False else checkFruit nextState nextLabel rest
  where
    nextState = findNewState fruitImpl currentState currentLabel

-- findNewState always takes the head when multiple transitions are possible
findNewState :: LTS -> State -> Label -> State
findNewState (_, _, transitions, _) currentState currentLabel =
  let transitionMatches = [newState | (start, label, newState) <- transitions, start == currentState, label == currentLabel]
   in if null transitionMatches then -1 else head transitionMatches
-- ===============================================================

-- == Problem 5 ==
corona1 r s x0 t = last.init $ scanl (\a b -> s+r*a) x0 [0..t]

corona2 r s x0 0 = x0 
corona2 r s x0 t = s + r * (corona2 r s x0 $ t-1)

corona3 r s x0 0 = s
corona3 r s x0 t = iterate ((s+).(r*)) x0 !! t

corona4 r s x0 t = (!! t) $ zipWith (+) (iterate (+s) 0) (iterate (*r) x0)

corona5 r s x0 t = r^t*x0 + s * (r^t-1)/(r-1)


-- Main
main :: IO ()
main = do
    -- quickCheck corefProp1
    quickCheck fibProp1
    quickCheck fibProp2
    quickCheck fibProp3

    print $ dispensesFruit ["?btn"]
    print $ dispensesFruit ["!apple"]
    print $ dispensesFruit ["!kiwi"]
    print $ dispensesFruit ["?btn", "!apple"]
    print $ dispensesFruit ["?btn", "!kiwi"]
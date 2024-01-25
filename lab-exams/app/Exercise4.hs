module Exercise4 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise1
import Exercise2
import Exercise3

-- Time spent: 1.5 hour

after :: IOLTS -> Trace -> [State]
after iolts@(states, _, _, transitions, start)  = after' iolts [start]

-- Keeps track of the currently reachable states, which get updated by
-- the output of this function. Recursively goes over the trace until
-- it is empty, and then returns the current reachable states.
after' :: IOLTS -> [State] -> Trace -> [State]
after' _ states [] = states
after' iolts states (x:xs) = after' iolts (afterTransition iolts states x) xs

-- Returns all states that can be reached from a set of given states.
afterTransition :: IOLTS -> [State] -> Label -> [State]
afterTransition iolts@(_, _, _, transitions, _) states label
    -- A delta transition can only be taken by quiescent states.
    | label == "delta" = filter (`elem` getAllQuiescent iolts) states
    -- Apply Tau transitions before and after looking at the possible transitions.
    | otherwise = afterTau transitions $ nub $ applyTransitions transitions (afterTau transitions states) label

-- Adds reachable states to the input set until it is stable.
afterTau :: [LabeledTransition] -> [State] -> [State]
afterTau transitions states
    | any (`notElem` states) newStates = afterTau transitions (nub (states ++ newStates))
    | otherwise = states
    where newStates = applyTransitions transitions states tau

-- Checks what states can be reached given a set of states and a label.
applyTransitions :: [LabeledTransition] -> [State] -> Label -> [State]
applyTransitions transitions states label = [to | (from, l, to) <- transitions, from `elem` states, l == label]

-- Checks if all states in the after are states within the IOLTS
prop_afterValidStates :: IOLTS -> Bool
prop_afterValidStates iolts@(states, _, _, _, _) = all (all (`elem` states) . after iolts) $ take 150 (straces iolts)

-- Checks if all states in the after are states within the IOLTS
prop_afterValidStates2 :: IOLTS -> Bool
prop_afterValidStates2 iolts@(states, _, _, _, _) = all (all (`elem` states) . after iolts) $ take 150 (traces2 iolts)

-- Checks if all states in the after are valid states to end up at given the last label
prop_afterPossibleStates :: IOLTS -> Bool
prop_afterPossibleStates iolts@(states, _, _, _, _) = all (statePossible iolts) $ take 150 (straces iolts)

-- Checks if all states in the after are valid states to end up at given the last label
prop_afterPossibleStates2 :: IOLTS -> Bool
prop_afterPossibleStates2 iolts@(states, _, _, _, _) = all (statePossible iolts) $ take 150 (traces2 iolts)

-- Checks if valid ends up with valid states after taking a certain trace
statePossible :: IOLTS -> Trace -> Bool
statePossible iolts@(states, _, _, transitions, start) trace
    -- If you take no steps, you will only ever be in the start state.
    | null trace = afterResult == [start]
    -- If the last label is delta, you must be in a quiescent state.
    | last trace == "delta" = all (`elem` getAllQuiescent iolts) (after iolts trace)
    -- Otherwise, you must be in one of states that the last label can lead to.
    | otherwise = all (`elem` possibleStates) (after iolts trace)
    where possibleStates = nub $ [t | (_, l, t) <- transitions, l == last trace]
          afterResult = after iolts trace

--   Test report:
-- ghci> main
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- +++ OK, passed 100 tests.
-- We can see that all our tests pass, which gives us reason to believe that
-- our after function is correct. However, we have only checked if the output
-- of `after` is possible, not whether the entire trace could lead to that
-- specific output. These were all the properties we could think of however,
-- and them not failing is a good sign.
main :: IO()
main = do
    quickCheck $ forAll ltsGenValid' prop_afterValidStates
    quickCheck $ forAll ltsGenValid' prop_afterValidStates2
    quickCheck $ forAll ltsGenValid' prop_afterPossibleStates
    quickCheck $ forAll ltsGenValid' prop_afterPossibleStates2

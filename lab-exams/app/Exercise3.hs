module Exercise3 where

import Data.List
import LTS
import Test.QuickCheck
import Exercise2  -- For the LTS generator

-- Time spent: 1.5 hour


-- Get all states of the IOLTS that could have quiescence,
-- thus get all states that do not have an output.
getAllQuiescent :: IOLTS -> [State]
getAllQuiescent (q, _, lu, lt, _) = filter (\x -> not $ elem x nonQuiStates) q
    where nonQuiStates = nub $ map (\ (x, y, z) -> x) (filter (\ (x, y, z) -> elem y lu) lt)

-- Get all possible transitions from the given states. A delta is also
-- possible as a transition if the state does not have any output.
nextTransitions'':: [State] -> [LabeledTransition]-> State -> [(State,Label)]
nextTransitions'' qs lt q0 =  [(s',l) | (s,l,s')<- lt , s == q0] ++ extra
    where extra = if elem q0 qs then [(q0, delta)] else []

-- Add the next transitions to the current straces to get the one step longer more straces
findfollowingtransitions'':: [State] -> [LabeledTransition] -> [State] -> [Label] -> [([State],[Label])]
findfollowingtransitions'' qs lt st ls = [(s:st,ls++[l])| (s,l)<-nextTransitions'' qs lt (head st)]

straces':: [LabeledTransition] -> [([State],[Label])]-> [State] -> [([State],[Label])]
straces' lt [] qs = []
straces' lt pairs qs = pairs ++ straces' lt next qs
    where next = concatMap (uncurry $ findfollowingtransitions'' qs lt) pairs

-- Get the straces with tau still in the straces.
stracesTau :: IOLTS -> [Trace]
stracesTau (q, li, lu, lt, q0) = nub $ map snd (straces' lt [([q0],[])] (getAllQuiescent (q, li, lu, lt, q0)))


-- Get the straces of the given IOLTS, straces are almost always infinite.
-- Straces and traces should not include any tau, thus these are removed.
-- Removing the tau could lead to duplicates, thus nub is done again.
straces :: IOLTS -> [Trace]
straces x = nub $ map (filter (/= tau)) (stracesTau x)

-- Takes IOLTS as an input, returns its traces.
traces2 :: IOLTS -> [Trace]
traces2 (q, li, lu, lt, q0) = traces (q, li ++ lu, lt, q0)

-- Random traces generator
-- We generate a random IOLTS, and then compute its straces.
stracesGen :: Gen [Trace]
stracesGen = do
    lts <- ltsGenValid'
    return $ straces lts


-- Test if the traces are also present in straces. This property should hold
-- because traces is a subset of straces. This is done for the first
-- 10 traces, since the amount of traces can be infinite. The time also
-- increases steeply when taking a higher number of traces.
-- Tau's in the traces are removed, since there should be no tau in a trace.
prop_traceInStrace :: IOLTS -> Bool
prop_traceInStrace x = all (\y -> elem y (straces x)) (removeTau (take 10 (traces2 x)))
    where removeTau y = nub $ map (filter (/= tau)) y

-- The empty string should be in the strace.
prop_emptyInStrace :: [Trace] -> Bool
prop_emptyInStrace x = elem [] x

-- Check if the lengths of the results of straces are increasing.
-- Breath first was implemented, thus the length sould be increasing.
prop_increasingSize :: [Trace] -> Bool
prop_increasingSize x = sizes == sort sizes
    where sizes = map length (take 100 x)

-- Make sure that the trace does not have any delta's direclty
-- before output labels.
checkDelta :: [Label] -> [LabeledTransition] -> Trace -> Bool
checkDelta lu lt [] = True
checkDelta lu lt (t:ts)
    | t == delta = (if ts == [] then True else not (elem (head ts) lu)) && (checkDelta lu lt ts)
    | otherwise = checkDelta lu lt ts

-- If the IOLTS does not contain any tau, then check that the first 100
-- straces do not have any delta's direclty before output labels,
-- We wanted to check it for all IOLTS, but it could legitimately happen
-- when a tau is used. Thus the property is only checked if the IOLTS
-- does not have any tau.
prop_noDeltaBeforeOut :: IOLTS -> Bool
prop_noDeltaBeforeOut x@(q, li, lu, lt, q0)
    | any (\ (_, x, _) -> x == tau) lt = error "The prop_noDeltaBeforeOut function can only take IOLTS that do not contain tau"
    | otherwise = all (checkDelta lu lt) (take 100 (straces x))

main :: IO()
main = do
    quickCheck $ forAll stracesGen prop_emptyInStrace
    quickCheck $ forAll stracesGen prop_increasingSize
    quickCheck $ forAll ltsGenValid' prop_noDeltaBeforeOut
    quickCheck $ forAll ltsGenValid' prop_traceInStrace
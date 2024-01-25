module Helpers where

import LTS
import Data.List
import Lecture2
import Test.QuickCheck

-- == Problem 2 ==
type Rel a = [(a, a)]

domain :: (Eq a) => Rel a -> [a]
domain r = map fst r `union` map snd r

composition :: (Ord a) => Rel a -> Rel a -> Rel a
composition r s = [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- Determine all rels on a domain A 
-- Note: this is a powerset of AxA
allRels :: (Ord a) => [a] -> Rel a
allRels domain = sort $ nub $ [(x,y) | x <- domain, y <- domain]

-- deltaA: creates a relation where every element is related to itself
-- Also called the identity relation
deltaA :: (Eq a) => [a] -> Rel a
deltaA xs = [(x, x) | x <- xs]

refClos :: (Eq a) => Rel a -> [a] -> Rel a
refClos r domain = r `union` deltaA domain

isReflexive :: (Eq a) => Rel a -> Bool
isReflexive r = all (\x -> (x, x) `elem` r) (domain r)

isIrreflexive :: (Eq a) => Rel a -> Bool
isIrreflexive r = not $ any (\x -> (x, x) `elem` r) (domain r)

isCoreflexive :: (Eq a) => Rel a -> Bool
isCoreflexive = all (\(x, y) -> x == y)

isSymmetric :: (Eq a) => Rel a -> Bool
isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

isAntisymmetric :: (Eq a) => Rel a -> Bool
isAntisymmetric r = all (\(x, y) -> (y, x) `elem` r --> x == y) r

isAsymmetric :: (Eq a) => Rel a -> Bool
isAsymmetric r = isIrreflexive r && isAntisymmetric r

inverse :: (Eq a) => Rel a -> Rel a
inverse r = [(y, x) | (x, y) <- r]

symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ nub $ r ++ inverse r

isTransitive :: (Eq a) => Rel a -> Bool
isTransitive r = and [(x, z) `elem` r | (x, y) <- r, (y', z) <- r, y == y']

-- From the Lab 4 exercises:
-- For each pair (x, y) in the first relation r, and each pair (w, z) in the second relation s,
-- if y equals w, then include (x, z) in the result.
-- Nub is then used to remove duplicates from the result.
infixr 5 @@

(@@) :: (Eq a) => Rel a -> Rel a -> Rel a
r @@ s = nub [(x, z) | (x, y) <- r, (w, z) <- s, y == w]

-- This function computes the transitive closure of a relation.
-- It does so by making use of the @@ operator defined above.
-- If the relation does not change after one step, it's the transitive closure.
-- Otherwise, just continue the process with the updated relation.
trClos :: (Eq a) => Rel a -> Rel a
trClos r
  | r == r' = r
  | otherwise = trClos r'
  where
    -- We perform a step of the transitive closure.
    -- Add the composed relation r @@ r to the original relation r and remove duplicates using nub.
    -- This means: r' = r U (r @@ r)
    r' = r `union` (r @@ r)

-- From the hints of exam 2018 
isSerial :: Eq a => [a] -> [(a, a)] -> Bool
isSerial s r = all (\x -> any (\y -> (x,y) `elem` r) s) s

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (\x -> elem x ys) xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

isEquivalence :: (Ord a) => Rel a -> [a] -> Bool
isEquivalence r domain = isReflexive r && isSymmetric r && isTransitive r

-- Determine minimal equivalence by doing the following: 
-- 1. Ensure that relation is reflexive
-- 2. Ensure that relation is symmetric
-- 3. Ensure that relation is transitive
minimalEquivalence :: (Ord a) => Rel a -> [a] -> Rel a
minimalEquivalence r domain = trClos $ symClos $ refClos r domain

-- To test: quickCheck $ forAll (arbitrary `suchThat` (> 0)) prop_notComparable
-- From exam question Olaf
prop_notComparable :: Int -> Bool
prop_notComparable n = not (subset lhs rhs) && not (subset rhs lhs)
  where
    a = [1 .. n]
    rels = subsets $ allRels a
    lhs = filter (\r -> isCoreflexive r && isTransitive r) rels
    rhs = filter (\r -> isAsymmetric r && isTransitive r) rels

-- Takes a domain and two properties of relations on that domain.
compareRelationProperties :: (Ord a) => [a] -> (Rel a -> Bool) -> (Rel a -> Bool) -> String
compareRelationProperties domain prop1 prop2 =
  let relations = subsets (allRels domain)
      prop1Relations = filter prop1 relations
      prop2Relations = filter prop2 relations
      prop1Stronger = all (\r -> prop1 r --> prop2 r) prop1Relations
      prop2Stronger = all (\r -> prop2 r --> prop1 r) prop2Relations
  in if prop1Stronger && prop2Stronger then "equivalent"
     else if prop1Stronger then "prop1 is stronger"
     else if prop2Stronger then "prop2 is stronger"
     else "incomparable"

-- Use abovementioned functions to check if a relation is coreflexive transitive 
isCoreflexiveTransitive :: Ord a => Rel a -> Bool
isCoreflexiveTransitive r  = isCoreflexive r && isTransitive r

-- Use abovementioned functions to check if a relation is asymmetric transitive
isAsymmetricTransitive :: Ord a => Rel a -> Bool
isAsymmetricTransitive r = isAsymmetric r && isTransitive r

-- Example prop for QuickCheck to be combined with compareRelationProperties
-- quickCheck $ forAll (nonEmptyListOf arbitrary) prop_incomparable
-- prop_incomparable :: [Int] -> Property
-- prop_incomparable domain = not (null domain) ==> compareRelationProperties domain isCoreflexiveTransitive isAsymmetricTransitive == "incomparable"

-- == Problem 4 ==
-- Get all states of the IOLTS that could have quiescence,
-- thus get all states that do not have an output.
getAllQuiescent :: IOLTS -> [State]
getAllQuiescent (q, _, lu, lt, _) = filter (\x -> x `notElem` nonQuiStates) q
    where nonQuiStates = nub $ map (\ (x, y, z) -> x) (filter (\ (x, y, z) -> y `elem` lu) lt)

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

-- LTS after function 
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

-- Checks if an LTS has any unreachable states.
-- This is done by using a breadth first search to find all reachable states.

-- Beide implementaties lijken te werken... 
-- Maar de eerste ziet er wat mooier uit
hasUnreachableStates :: LTS -> Bool
hasUnreachableStates (states, _, transitions, start) =
    any (`notElem` reachableStates) states
    where
        reachableStates = bfs [start] []

        bfs :: [State] -> [State] -> [State]
        bfs [] _ = []
        bfs (s:ss) visited
            | s `elem` visited = bfs ss visited
            | otherwise = s : bfs (ss ++ nextStates s) (s:visited)

        nextStates :: State -> [State]
        nextStates s = [s' | (from, _, s') <- transitions, from == s]


-- hasUnreachableStates :: LTS -> Bool
-- hasUnreachableStates lts@(states, _, transitions, start) = 
--     any (`notElem` reachableStates) states
--     where
--         reachableStates = bfs [start] [] transitions

--         bfs :: [State] -> [State] -> [LabeledTransition] -> [State]
--         bfs [] visited _ = visited
--         bfs (current:queue) visited transitions = 
--             let newStates = nub [s' | (s,l,s') <- transitions, s == current, s' `notElem` visited]
--                 visited' = current : visited
--                 queue' = queue ++ newStates
--             in bfs queue' visited' transitions

-- Checks if a set of states refuses all the given labels.
-- The `refuses` function determines if a set of states within an LTS (Labeled Transition System)
-- collectively refuse a given set of labels. A state refuses a label if it has no outgoing
-- transition for that label. If all the states refuse all the given labels, the function returns True.
-- Otherwise, it returns False.
--
-- @param (_, _, transitions, _) The full LTS, with transitions being the list of transitions.
-- @param states The list of states being checked for refusals.
-- @param labels The list of labels being checked for refusals.
-- @return A Boolean indicating whether the set of states refuses all the labels.
refuses :: LTS -> [State] -> [Label] -> Bool
refuses (_, _, transitions, _) states labels =
  all (\s -> all (\l -> not (elem (s, l) possibleTransitions)) labels) states
  where
    possibleTransitions = [(from, l) | (from, l, _) <- transitions]

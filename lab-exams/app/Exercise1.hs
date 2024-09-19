module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck

-- Time spent: 1 hour

-- List of factors that result in invalid IOLTSs:
-- Q can be empty.
-- q0 might not be in Q.
-- One of the states of a transition might not be in Q.
-- The label of a transition might not be in L union tau.
-- The input labels and output labels might not be disjoint.
-- Not all inputs might be enabled in all states.

main :: IO ()
main = print $ validateLTS counterImpl

-- Checks whether an IOLTS is valid
validateLTS :: IOLTS -> Bool
validateLTS (q, l_i, l_o, t, q_0) =
    not (null q) -- Q must not be empty
    && q_0 `elem` q -- q_0 must be in Q
    && validTransitions q l_i l_o t q_0 -- States must be in Q, and labels in L union tau
    && null (intersect l_i l_o) -- L_i and L_o must be disjoint
    -- The line below checks whether all inputs are enabled in all states.
    -- However, according to the announcement, we do not have to check this.
    -- && all (\s -> all (\l -> any (\(s_0, l', s_1) -> 
    --    s == s_0 && l == l') t) l_i) q
    where validTransitions q l_i l_o t q_0 =
            all (\(s, l, s') -> 
                s `elem` q 
                && s' `elem` q 
                && l `elem` (l_i ++ l_o ++ [tau])) t

-- Test properties

-- An LTS without states is invalid.
-- We test this by making Q empty.
prop_noStates :: IOLTS -> Bool
prop_noStates (q, l_i, l_o, t, q_0) = not $ validateLTS ([], l_i, l_o, t, q_0)

-- An LTS with a transition having a label not in L union tau is invalid.
-- We test this by making L empty and adding a transition with label 'test'.
prop_invalidLabel :: IOLTS -> Bool
prop_invalidLabel (q, l_i, l_o, t, q_0) = not $ validateLTS (q, [], [], (q_0,"test",q_0):t, q_0)

-- An LTS with q0 not in Q is invalid.
-- We test this by having only one state, 
-- which is unequal to q0 (namely q0 + 1).
prop_invalidQ0 :: IOLTS -> Bool
prop_invalidQ0 (q, l_i, l_o, t, q_0) = not $ validateLTS ([q_0+1], l_i, l_o, t, q_0)

-- An LTS with L_i and L_o not disjoint is invalid.
-- We test this by adding an element both to L_i and L_o.
prop_notDisjoint :: IOLTS -> Bool
prop_notDisjoint (q, l_i, l_o, t, q_0) = not $ validateLTS (q, "test":l_i, "test":l_o, t, q_0)

-- An LTS with a transition from a state not in Q is invalid.
-- We test this by having only one state (q_0),
-- and creating a transition from another state (q_0 + 1).
prop_invalidState :: IOLTS -> Bool
prop_invalidState (q, l_i, l_o, t, q_0) = not $ validateLTS ([q_0], l_i, l_o, (q_0+1,"test",q_0):t, q_0)

-- An LTS with only states (no transitions) is valid,
-- as long as q_0 is in Q.
prop_onlyStates :: IOLTS -> Bool
prop_onlyStates (q, l_i, l_o, t, q_0) = validateLTS (q_0:q, [], [], [], q_0)

-- A valid LTS should be recognized as valid.
-- For this property, we need a generator of valid LTS.
prop_validLTS :: IOLTS -> Bool
prop_validLTS = validateLTS

-- Test report:
-- We will only execute tests in the next exercise,
-- after which we will have generators.
-- 
-- We created many properties with invalid LTSs.
-- Therefore, we are confident we can catch invalid LTSs.
-- We also have a property for valid LTSs.
-- However, this one is dependent on the correctness
-- of the generator we will write in the next exercise.

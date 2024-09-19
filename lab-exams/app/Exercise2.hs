module Exercise2 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise1

-- Time spent: 1 hour

-- Execute all tests
main :: IO ()
main = do
    quickCheck $ forAll ltsGenValid' prop_noStates
    quickCheck $ forAll ltsGenValid' prop_invalidLabel
    quickCheck $ forAll ltsGenValid' prop_invalidQ0
    quickCheck $ forAll ltsGenValid' prop_notDisjoint
    quickCheck $ forAll ltsGenValid' prop_invalidState
    quickCheck $ forAll ltsGenValid' prop_onlyStates
    quickCheck $ forAll ltsGenValid' prop_validLTS

-- Generate a truly random IOLTS. While this can theoretically generate
-- valid IOLTSs, the chance of this happening is very small.
ltsGen :: Gen IOLTS
ltsGen = do
    states <- listOf1 arbitrary
    labels1 <- listOf1 arbitrary
    labels2 <- listOf1 arbitrary
    transitions <- listOf1 arbitrary
    initial <- arbitrary
    return (states, labels1, labels2, transitions, initial)

-- Generate a positive integer
genPos :: Gen Integer
genPos = abs <$> (arbitrary :: Gen Integer) `suchThat` (> 0)

-- Generate a set of positive integers
genIntSet :: Gen [Integer]
genIntSet = listOf1 genPos `suchThat` (\x -> length x == length (nub x))

-- Generate a random transition from a set of labels and states
genTransition :: [Label] -> [Integer] -> Gen LabeledTransition
genTransition labels states = do
    from <- elements states
    to <- elements states
    label <- elements labels
    return (from, label, to)

-- Generate a valid IOLTS
ltsGenValid :: Gen IOLTS
ltsGenValid = do
    states <- genIntSet
    labels1 <- listOf1 arbitrary
    labels2 <- listOf1 arbitrary `suchThat` (\x -> null (x `intersect` labels1))
    transitions <- listOf1 (genTransition (labels1 ++ labels2) states)
    initial <- elements states
    return (states, labels1, labels2, transitions, initial)

-- Alternatively, we can implement an IOLTS by using the provided createIOLTS
ltsGen' :: Gen IOLTS
ltsGen' = do
    transitions <- listOf1 arbitrary
    return (createIOLTS transitions)

genTransition' :: [Label] -> [Label] -> [Integer] -> Gen LabeledTransition
genTransition' labels1 labels2 states = do
    let labels = labels1 ++ labels2
    from <- elements states
    to <- elements states
    label <- elements labels
    let prefix = if label `elem` labels1 then "?" else "!"
    return (from, prefix ++ label, to)

-- Same for the valid version
ltsGenValid' :: Gen IOLTS
ltsGenValid' = do
    labels1 <- listOf1 arbitrary
    labels2 <- listOf1 arbitrary `suchThat` (\x -> null (x `intersect` labels1))
    states <- genIntSet
    transitions <- listOf1 (genTransition' labels1 labels2 states)
    return (createIOLTS transitions)

-- Generates readable strings
genBeautifulString :: Gen String
genBeautifulString = do
    n <- genPos
    vectorOf (fromIntegral n) (elements ['a'..'z'])

-- Generates LTS with readable labels
ltsGenValidBeautiful :: Gen IOLTS
ltsGenValidBeautiful = do
    labels1 <- listOf1 genBeautifulString
    labels2 <- listOf1 genBeautifulString `suchThat` (\x -> null (x `intersect` labels1))
    states <- genIntSet
    transitions <- listOf1 (genTransition' labels1 labels2 states)
    return (createIOLTS transitions)

-- Generates LTS including tau
ltsGenValidTau :: Gen IOLTS
ltsGenValidTau = do
    labels1 <- listOf1 arbitrary
    labels2 <- listOf1 arbitrary `suchThat` (\x -> null (x `intersect` labels1))
    states <- genIntSet
    transitions <- listOf1 (genTransition' labels1 (tau:labels2) states)
    return (createIOLTS transitions)

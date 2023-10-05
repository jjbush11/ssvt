module Namaak where 

getResultsExtra :: [[Integer] -> Gen [Integer]]
                -> (Integer -> [Integer])
                -> [[Integer] -> Integer -> Bool]
                -> Int 
                -> Int
                -> Results a
getResultsExtra mutators f properties nMutants nTests = map (uncurry $ processRawResult mex nts)
                                 $ getRawResults is pmap mutators
  where is = [1..(length $ properties f)]
        pmap f = propertiesToMap (properties f) nTests
        ms' = take (nms+1) (tail $ mutants f)
        mex = length ms' <= nMutants
        ms = take nms ms' ++ ems

propertiesToMap :: [Property] -> Int -> [Bool]
propertiesToMap ps n = map (propertyHolds n) ps

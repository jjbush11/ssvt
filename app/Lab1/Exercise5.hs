module Lab1.Exercise5 where

import Test.QuickCheck
import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Matthew: Carl didn't do it, and neither did I.
-- Peter: It was Matthew or it was Jack.
-- Jack: Matthew and Peter are both lying.
-- Arnold: Matthew or Peter is speaking the truth, but not both.
-- Carl: What Arnold says is not true.

-- Encodes whether a boy accuses another boy.
accuses :: Boy -> Boy -> Bool
-- Matthew: Carl didn't do it, and neither did I.
accuses Matthew boy = boy /= Carl && boy /= Matthew
-- Peter: It was Matthew or it was Jack.
accuses Peter boy = boy == Matthew || boy == Jack
-- Jack: Matthew and Peter are both lying.
accuses Jack boy = not (accuses Matthew boy) && not (accuses Peter boy)
-- Arnold: Matthew or Peter is speaking the truth, but not both.
accuses Arnold boy = (accuses Matthew boy || accuses Peter boy) && not (accuses Matthew boy && accuses Peter boy)
-- Carl: What Arnold says is not true.
accuses Carl boy = not (accuses Arnold boy)

accusers :: Boy -> [Boy]
accusers boy = [accuser | accuser <- boys, accuses accuser boy]

-- Determine the honest boys based on the teacher's statement
-- TO-DO: Doesn't work correctly
honest :: [Boy]
honest = filter (\boy -> length (accusers boy) == 2) boys

-- Determine the guilty boy based on the accusations and the teacher's statement
guilty :: [Boy]
guilty =
    let liars = filter (\boy -> length (accusers boy) == 2) boys
        possibleGuilty = nub (concatMap accusers liars) \\ liars
    in if length possibleGuilty == 1 then possibleGuilty else []

-- Exercise 5

main :: IO ()
main = do
  print guilty
  print honest
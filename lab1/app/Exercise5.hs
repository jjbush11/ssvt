module Exercise5 where

import Data.List
import Test.QuickCheck

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

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
-- accuses Arnold boy = (accuses Matthew boy || accuses Peter boy) && not (accuses Matthew boy && accuses Peter boy)
accuses Arnold boy = xor (accuses Matthew boy) (accuses Peter boy)
-- Carl: What Arnold says is not true.
accuses Carl boy = not (accuses Arnold boy)

accusers :: Boy -> [Boy]
accusers boy = [accuser | accuser <- boys, accuses accuser boy]

-- Determine the honest boys based on the teacher's statement
-- Based on the output we expect that ["Matthew", "Peter", "Carl"] would be the outcome of honest
honest :: [Boy]
honest = [boy | boy <- boys, isHonest boy]
  where
    isHonest boy =
      let accusersOfBoy = accusers boy
       in length accusersOfBoy == 2

-- Determine the guilty boy based on the accusations and the teacher's statement
guilty :: [Boy]
guilty = [boy | boy <- boys, isGuilty boy]
  where
    isGuilty boy =
      let accusersOfBoy = accusers boy
       in length accusersOfBoy /= 2

-- Exercise 5
main :: IO ()
main = do
  -- Guility gives us Jack as the person who did it.
  -- Based on the accuses this is correct.
  print guilty
  -- Honest provides us with a list of [Matthew,Peter,Arnold,Carl]
  -- This includes Arnold but this is incorrect.
  -- Since Arnold said that not both Matthew and Peter are telling the truth.
  -- But they are both in the list of honest poeple.
  print honest
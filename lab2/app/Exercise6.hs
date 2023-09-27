module Exercise6 where

import Test.QuickCheck
import LTS
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.Text.Lazy.IO as TIO 
import qualified Data.Text.Lazy as TL

convertLTS :: ([Integer], [(Integer, String, Integer)]) -> Data.GraphViz.DotGraph Integer
convertLTS (states, transitions) =
  let nodes = convertNodesToFormat states
      edges = map convertTransition transitions
  in graphElemsToDot params (convertNodesToFormat states) edges
  where
    params = nonClusteredParams { fmtNode = \(_,label) -> [textLabel label] }
    convertNodesToFormat = map (\state -> (state, TL.pack $ show state))
    convertTransition (from, label, to) = (from, to, label)

ltsToDotGraph :: IOLTS -> DotGraph Integer
ltsToDotGraph (states, _, _, transitions, _) =
  convertLTS (states, transitions)

-- A function which prints a graph to the terminal
printGraph :: DotGraph Integer -> IO ()
printGraph = TIO.putStrLn . printDotGraph


main :: IO ()
main = do
  printGraph $ ltsToDotGraph coffeeImpl1

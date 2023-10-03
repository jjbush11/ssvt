module Exercise6 where

import Test.QuickCheck
import LTS
import Data.GraphViz
import Data.GraphViz.Printing
import Data.Graph.Inductive
import Exercise2


-- For this to work you need to install the graphviz library
-- On Mac: brew install graphviz
-- On Ubuntu: sudo apt-get install graphviz
-- On Windows: https://graphviz.gitlab.io/_pages/Download/Download_windows.html
-- Furthermore, the GraphViz bindings package from hackage needs to be installed

-- Convert an IOLTS to a graph, which makes use of mkGraph from the fgl library
convertIOLTSToGraph :: IOLTS -> Gr State Label
convertIOLTSToGraph (states, inputLabels, outputLabels, transitions, initialState) = mkGraph nodes edges
  where
    nodes = zip [1..] states
    edges = map (\(s, label, s') -> (fromIntegral s, fromIntegral s', label)) transitions

-- Convert graph to dot graph
dotGraph :: Gr State Label -> DotGraph Node
dotGraph graph = graphToDot params graph
  where
    -- These params are necessary to render the labels for the transitions correctly
    params = nonClusteredParams { fmtEdge = \(_, _, label) -> [toLabel label] }

-- Visualize an IOLTS by converting it to a graph and then rendering it to an image
visualizeIOLTS :: IOLTS -> FilePath -> IO FilePath
visualizeIOLTS graph outputFilePath = do
  -- Use Graphviz functions to render the graph directly to an image
  let format = Png  -- You can choose a different format if needed
  -- Render the graph to the specified format and save it
  runGraphviz (dotGraph $ convertIOLTSToGraph graph) format outputFilePath

-- Visualize a random IOLTS
-- A generator can be passed to this function to generate a specific IOLTS
visualizeRandomIOLTS :: Gen IOLTS -> FilePath -> IO FilePath
visualizeRandomIOLTS gen outputFilePath = do
  graph <- generate gen
  visualizeIOLTS graph outputFilePath

main :: IO ()
main = do
  visualizeRandomIOLTS randomIOLTS "randomIOLTS.png"
  visualizeRandomIOLTS allPossibleTransitionslIOLTS "allPossibleTransitionslIOLTS.png"
  print "New IOLTS generated and visualized"

-- Time Spent: 1 hour
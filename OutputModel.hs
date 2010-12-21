{- 
   Author: Ka Wai
   License: GPL 3.0
   File: OutputModel.hs
   Description: outputs a graph representing the given model
-}

module OutputModel where 

import Data.List
import Data.Map
import System.Cmd
import System.Directory

import Signature
import Model
import ProofSearch

-- Creates visual graph of model in specified format
outputModel :: Model -> FilePath -> String -> IO ()
outputModel model filename format
  = do writeModel (filename ++ ".dot") model
       rawSystem "dot" ["-T" ++ format, fp ++ ".dot", "-o", fp ++ "." ++ format]
       removeFile (fp ++ ".dot")
       return ()
  where fp = "models/" ++ filename

{-
 Output string of DOT language that draws model
 WARNING: assumes the model is already checked & correct
 Returns message if same relation name listed more than once
-}
modelToGraph :: Model -> String
modelToGraph ([], _, _)
  = "digraph {\n label = \"Domain is empty, no model to draw\" ;\n}"
modelToGraph (dom, us, bs)
  | length dom /= length (nub dom) = 
      begin ++ "label = \"Domain contains duplicated individuals\" ;\n" ++ end
  | not $ isUnique us =
      begin ++ "label = \"Duplicated unary relation names exist\" ;\n" ++ end
  | not $ isUnique bs =
      begin ++ "label = \"Duplicated binary relation names exist\" ;\n" ++ end
  | otherwise =
      begin ++ concat ulabels ++ domOnlyToGraph (dom Data.List.\\ unodes) ++
      concatMap drawEdges bs ++ end
  where (unodes, ulabels) = unaryToGraph $ mapUnary us empty
        begin = "digraph {\n "
        end = "}"

-- Returns true if there is only 1 occurrence of the fst of the pairs in the list
isUnique :: [(String, a)] -> Bool
isUnique xs = length xs == length (nub $ fst $ unzip xs)

-- Draws edges for a binary relation
drawEdges :: (String, [(Individual, Individual)]) -> String
drawEdges (_, [])       = []
drawEdges (r, (p,c):es)
  = concat [show p, " -> ", show c, " [label=\"", r, "\"] ;\n ",
    drawEdges (r, nub es Data.List.\\ [(p,c)])]

-- Returns string that draws all individuals that are not in unary relations
domOnlyToGraph :: [Individual] -> String
domOnlyToGraph ds = concat [show i ++ " [label=\"" ++ show i ++ "\"] ;\n " | i <- ds]

-- Returns string to label every unary in the 
unaryToGraph :: Map Individual String -> ([Individual], [String])
unaryToGraph m
  = unzip [(k, show k ++ " [label=\"" ++ show k ++ ": " ++ addnewlines a ++
            "\"] ;\n ") | (k, a) <- Data.Map.toList m ]

-- Ensures node is not too large
-- Adds a newline after every 9 characters
addnewlines :: String -> String
addnewlines cs
  | line `elem` [[], " "] = []
  | rest `elem` [[], " "] = line
  | otherwise             = line ++ "\\n" ++ addnewlines rest
  where (line, rest) = splitAt 6 cs

-- Collects all dot labels
mapUnary :: [UnaryRelation] -> Map Individual String -> Map Individual String
mapUnary [] m          = m
mapUnary ((u,is):xs) m = mapUnary xs $ addToIndividual u is m

-- Add unary to all individuals satisfying
addToIndividual :: String -> [Individual] -> Map Individual String
                   -> Map Individual String
addToIndividual u [] m     = m
addToIndividual u (i:is) m = insertWith (++) i (u ++ " ") $ addToIndividual u is m

-- Writes graph to file.
writeModel :: FilePath -> Model -> IO()
writeModel filename = writeFile ("models/"++filename) . modelToGraph
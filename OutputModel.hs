{- 
   Author: Ka Wai
   License: GPL 3.0
   File: OutputModel.hs
   Description: outputs a graph representing the given model
-}

module OutputModel where 

import Data.List
import qualified Data.Map as Map
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
      begin ++ concat ulabels ++ domOnlyToGraph (dom \\ unodes) ++
      concatMap drawEdges bs ++ end
  where (unodes, ulabels) = unaryToGraph $ mapUnary us Map.empty
        begin = "digraph {\n "
        end = "}"

-- Returns true if there is only 1 occurrence of the fst of the pairs in the list
isUnique :: [(String, a)] -> Bool
isUnique xs = length xs == length (nub . fst $ unzip xs)

-- Draws edges for a binary relation
drawEdges :: (String, [(Individual, Individual)]) -> String
drawEdges (_, [])       = []
drawEdges (r, (p,c):es)
  = concat [show p, " -> ", show c, " [label=\"", r, "\"] ;\n ",
            drawEdges (r, nub es \\ [(p,c)])]

-- Returns string that draws all individuals that are not in unary relations
domOnlyToGraph :: [Individual] -> String
domOnlyToGraph ds
  = concat [show i ++ " [label=\"" ++ show i ++ "\"] ;\n " | i <- ds]

-- Returns string to label every unary in the 
unaryToGraph :: Map.Map Individual [String] -> ([Individual], [String])
unaryToGraph m
  = unzip [(k, show k ++ " [label=\"" ++ show k ++ ": " ++
            intercalate ", " (addnewlines a) ++ "\"] ;\n ")
           | (k, a) <- Map.toList m ]

-- Ensures node is not too large
-- Adds a newline after every 8 characters (specified by max's assigned value)
addnewlines :: [String] -> [String]
addnewlines [] = []
addnewlines (c:cs)
  | index == 0 = (pre ++ "-\\n" ++ newsuf) : newtail
  | rest == [] = c:cs
  | otherwise  = line ++ (("\\n" ++ head newrest) : tail newrest)
  where (line, rest) = splitAt index (c:cs)
        acculength = scanl1 ((+) . (+2)) $ map length (c:cs)
--        acculength = zipWith (+) [2,4..] $ scanl1 (+) $ map length (c:cs)
        index = length $ takeWhile (<=max) acculength
        (pre, suf) = splitAt (max-1) c
        max = 8
        (newsuf:newtail) = addnewlines (suf:cs)
        newrest = addnewlines rest

-- Collects all dot labels
mapUnary :: [UnaryRelation] -> Map.Map Individual [String]
            -> Map.Map Individual [String]
mapUnary [] m          = m
mapUnary ((u,is):xs) m = mapUnary xs $ addToIndividual u is m

-- Add unary to all individuals satisfying
addToIndividual :: String -> [Individual] -> Map.Map Individual [String]
                   -> Map.Map Individual [String]
addToIndividual u [] m     = m
addToIndividual u (i:is) m = Map.insertWith (++) i [u] $ addToIndividual u is m

-- Writes graph to file.
writeModel :: FilePath -> Model -> IO()
writeModel filename = writeFile ("models/"++filename) . modelToGraph
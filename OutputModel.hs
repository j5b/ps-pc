{- 
   Author: Ka Wai
   License: GPL 3.0
   File: OutputModel.hs
   Description: outputs a graphviz file representing the given model
-}

module OutputModel where 

import Data.List
import Data.Map

import Signature
import Model
import ProofSearch

{-
 Output string of DOT language that draws model
 WARNING: assumes the model is already checked & correct
 Returns message if same relation name listed more than once
-}
modelToGraph :: Model -> String
modelToGraph ([], _, _) = "Domain is empty, no model to draw"
modelToGraph (dom, us, bs)
  | (length dom) /= (length $ nub dom) = 
      "Domain contains duplicated individuals"
  | not $ isUnique us =
      "Duplicated unary relation names exist"
  | not $ isUnique bs =
      "Duplicated binary relation names exist"
  | otherwise =
      "digraph {\n " ++ concat ulabels ++
      domOnlyToGraph (dom Data.List.\\ unodes) ++ concatMap drawEdges bs ++ "}"
  where (unodes, ulabels) = unaryToGraph $ mapUnary us empty

-- Returns true if there is only 1 occurrence of the fst of the pairs in the list
isUnique :: [(String, a)] -> Bool
isUnique xs = (length xs) == (length $ nub $ fst $ unzip xs)

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
  = unzip [(k, show k ++ " [label=\"" ++ show k ++ ": " ++ a ++ "\"] ;\n ")
           | (k, a) <- Data.Map.toList m ]

-- Collects all dot labels
mapUnary :: [UnaryRelation] -> Map Individual String -> Map Individual String
mapUnary [] m          = m
mapUnary ((u,is):xs) m = mapUnary xs $ addToIndividual u is m

-- Add unary to all individuals satisfying
addToIndividual :: String -> [Individual] -> Map Individual String
                   -> Map Individual String
addToIndividual u [] m     = m
addToIndividual u (i:is) m = insertWith (++) i (u ++ " ") $ addToIndividual u is m

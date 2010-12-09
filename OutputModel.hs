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


-- Strategy: draw unaries then draw elements in domain that do not appear in unaries, then draw the relations
modelToGraph :: Model -> String
modelToGraph ([], _, _) = ""
modelToGraph (dom, us, bs)
  = "digraph { \n" ++ unaryToGraph (mapUnary us empty) ++
    domOnlyToGraph dom us ++ relToGraph bs ++ " }"

-- TO DO: Draw relations
relToGraph :: [BinaryRelation] -> String
relToGraph [] = []
relToGraph ((r, edges):rs) = drawEdges r edges ++ relToGraph rs

-- Draws edges for a binary relation
drawEdges :: String -> [(Individual, Individual)] -> String
drawEdges r []         = []
drawEdges r ((p,c):es) = concat [show p, " -> ", show c, " [label = \"", r, "\"] ;\n ", drawEdges r es]

-- Returns string that draws all individuals that are not in unary relations
domOnlyToGraph :: [Individual] -> [UnaryRelation] -> String
domOnlyToGraph ds us = concat [show x ++ ";\n" | x <- xs]
  where xs = domOnly ds us

-- Returns individuals that do not have unary relations
domOnly :: [Individual] -> [UnaryRelation] -> [Individual]
domOnly [] _           = []
domOnly d []           = d
domOnly d ((_,is):xs)  = domOnly (d Data.List.\\ is) xs

-- Returns string to label every unary in the 
unaryToGraph :: Map Individual String -> String
unaryToGraph m = "\\* Nodes for individuals *\\ \n " ++ concat [show k ++ " [label=\"" ++ show k ++ ": " ++ a ++ "\"] ;\n" | (k, a) <- list ]
 where list = Data.Map.toList m

-- Collects all dot labels
mapUnary :: [UnaryRelation] -> Map Individual String -> Map Individual String
mapUnary [] m          = m
mapUnary ((u,is):xs) m = mapUnary xs $ addToIndividual u is m

-- Add unary to all individuals satisfying
addToIndividual :: String -> [Individual] -> Map Individual String
                   -> Map Individual String
addToIndividual u [] m     = m
addToIndividual u (i:is) m = insertWith (++) i (u ++ " ") $ addToIndividual u is m
 

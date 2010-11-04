{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: checks if a model is valid for a given concepts
-}

module Model.Checker where 

import Data.Maybe
import Control.Monad
import Signature
import Model
import Report

type InputModel = (Model, [Concept], [Concept])
type Distiguished = Maybe Individual

-- returns the knowledge base considered
getKB :: InputModel -> [Concept]
getKB (_, _, kb) = kb

-- returns the given concepts
getGC :: InputModel -> [Concept]
getGC (_, gc, _) = gc

getConcepts :: InputModel -> [Concept]
getConcepts input = (getKB kb) ++ (getGC gc)

-- TODO: What to do if the model is empty
{-
To check if a model is valid
  - 1st : Put all concepts to NNF form
  - 2nd : Choose an individual
  - 3rd : Check the validity for each concept in given model
      - Check atomic formulas one by one
      - TODO: Find out what to do for forall and exists
      - Output report for each
  - 4th : Output the report (String, Bool)
-}
allConceptsToNNF :: [Concept] -> [Concept]
allConceptsToNNF = map toNNF

-- TODO: Modify this so it includes reports
checkConcept :: Dintinguished -> Concept -> Model -> Bool
-- Assuming that in NNF only not atoms occurs but not not concepts
checkConcept individual (And f1 f2) model = 
  f1 && f2
  where result1 = if isAtomic f1 
                     then checkAtomic individual f1 model
                     else checkConcept individual f1 model
        result2 = if isAtomic f2 
                     then checkAtomic individual f2 model
                     else checkConcept individual f2 model

-- checkAtomicReport :: Distinguished -> Concept -> Model -> Bool

-- TODO: Test this function
checkAtomic :: Distinguished -> Concept -> Model -> Bool
checkAtomic _ T _ = True
checkAtomic _ (Neg T) _ = False
checkAtomic individual (Atom atom) model 
  | isNothing individual = False
  | otherwise           = isInUnary atom (fromJust individual) model
checkAtomic individual (Neg (Atom atom)) model
  | isNothing individual = True
  | otherwise           = not $ isInUnary atom (fromJust individual) model

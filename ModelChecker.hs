{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: checks if a model is valid for a given concepts
-}

module ModelChecker (checkAtomic, checkConcept, checkModel, InputModel) where 

{-
To check if a model is valid
  - 1st : Put all concepts to NNF form
  - 2nd : Choose an individual
  - 3rd : Check the validity for each concept in given model
      - Check atomic formulas one by one
      - Output report for each
  - 4th : Output the report (String, Bool)
  TODO: reports
-}

import Data.Maybe
import Control.Monad
import Signature
import Model
import Report

type KnowledgeBase = [Concept]
type Givens = [Concept]
type Concepts = [Concept]
type InputModel = (Model, Givens, KnowledgeBase)

-- return the model
getModel :: InputModel -> Model
getModel (model, _, _) = model

-- returns the knowledge base considered
getKB :: InputModel -> KnowledgeBase
getKB (_, _, kb) = kb

-- returns the given concepts
getGC :: InputModel -> Givens
getGC (_, gc, _) = gc

getConcepts :: InputModel -> Concepts
getConcepts input = (getKB input) ++ (getGC input)

-- checks if input model is fine REALLY IMPORTANT FUNCTION
-- TODO: TEST THIS
{- checkInputModel :: InputModel -> Bool
checkInputModel input = 
  and $ map (flip checkModel model) concepts
  where model    = getModel input
        concepts = map toNNF $ getConcepts input 
-}

-- check if model is a model for concept
checkModel :: Concept -> Model -> Bool
{-
  Laziness will prevent to check the concept for every model
  Once one element validates the model it terminates :)
-}
checkModel _ ([], _, _ ) = False
checkModel concept model =
  or $ map (checkConcept concept model) $ getDomain model

-- TODO: Modify this so it includes reports
checkConcept :: Concept -> Model -> Individual -> Bool
-- Assuming that in NNF only not atoms occurs but not not concepts
checkConcept (And f1 f2) model distinguished  = 
  result1 && result2
  where result1 = checkConcept f1 model distinguished
        result2 = checkConcept f2 model distinguished
checkConcept (Or f1 f2) model distinguished =
  result1 || result2
  where result1 = checkConcept f1 model distinguished
        result2 = checkConcept f2 model distinguished
checkConcept (Exists relation f) model distinguished =
  or $ map (checkConcept f model) $ map snd $ filter (matches distinguished) relationSet
  where matches individual x = (fst x) == individual
-- TODO: I don't like the fromJust bit find something better
        relationSet          = fromJust $ lookup relation relations
        relations            = getRelations model
checkConcept (Forall relation f) model distinguished =
  and $ map (checkConcept f model) $ map snd $ filter (matches distinguished) relationSet
  where matches individual x = (fst x) == individual
-- TODO: I don't like the fromJust bit find something better
        relationSet          = fromJust $ lookup relation relations
        relations            = getRelations model
checkConcept any model distinguished =
   -- if all cases fail we have an atom
   checkAtomic any model distinguished 

-- Check if an atomic concept works for a distinguished individual (or no one) in a model
checkAtomic :: Concept -> Model -> Individual -> Bool
checkAtomic _ ([], _, _) _ = False -- this deals with the unlikely case of an empty model
checkAtomic T _ _ = True
checkAtomic (Neg T) _ _ = False
checkAtomic (Atom atom) model distinguished =
  isInUnary distinguished atom model
checkAtomic (Neg (Atom atom)) model distinguished =
  not $ isInUnary distinguished atom model

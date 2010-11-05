{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: checks if a model is valid for a given concepts
-}

module ModelChecker (checkAtomic, checkConcept, checkModel, InputModel) where 

import Data.Maybe
import Control.Monad
import Signature
import Model
import Report

type KnowledgeBase = [Concept]
type Givens = [Concept]
type Concepts = [Concept]
type InputModel = (Model, Givens, KnowledgeBase)

-- returns the knowledge base considered
getKB :: InputModel -> KnowledgeBase
getKB (_, _, kb) = kb

-- returns the given concepts
getGC :: InputModel -> Givens
getGC (_, gc, _) = gc

getConcepts :: InputModel -> Concepts
getConcepts input = (getKB input) ++ (getGC input)

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
allConceptsToNNF :: Concepts -> Concepts
allConceptsToNNF = map toNNF

{-
  If overall a and b fails and a and/or b are atomic, state 
  the formula fails for distinguished on atom a or b

  For each individual in domain check if concept works or not
  If the concept never works then combine reports to state which
  formula fails in each case
-}


checkModel :: Concept -> Model -> Bool
{-
  Laziness will prevent to check the concept for every model
  Once one element validates the model it terminates :)
-}
checkModel _ ([], _, _ ) = False
checkModel concept model =
  or $ result 
  where result = [ checkConcept x concept model | x <- (getDomain model) ]

-- TODO: Modify this so it includes reports
checkConcept :: Individual -> Concept -> Model -> Bool
-- Assuming that in NNF only not atoms occurs but not not concepts
checkConcept distinguished (And f1 f2) model = 
  result1 && result2
  where result1 = if isAtomic f1 
                     then checkAtomic distinguished f1 model
                     else checkConcept distinguished f1 model
        result2 = if isAtomic f2 
                     then checkAtomic distinguished f2 model
                     else checkConcept distinguished f2 model
checkConcept distinguished (Or f1 f2) model =
  result1 || result2
  where result1 = if isAtomic f1 
                     then checkAtomic distinguished f1 model
                     else checkConcept distinguished f1 model
        result2 = if isAtomic f2 
                     then checkAtomic distinguished f2 model
                     else checkConcept distinguished f2 model
checkConcept distinguished any model =
   -- if all cases fail we have an atom
   checkAtomic distinguished any model

-- Check if an atomic concept works for a distinguished individual (or no one) in a model
checkAtomic :: Individual -> Concept -> Model -> Bool
checkAtomic _ _ ([], _, _) = False -- this deals with the unlikely case of an empty model
checkAtomic _ T _ = True
checkAtomic _ (Neg T) _ = False
checkAtomic distinguished (Atom atom) model =
  isInUnary distinguished atom model
checkAtomic distinguished (Neg (Atom atom)) model =
  not $ isInUnary distinguished atom model

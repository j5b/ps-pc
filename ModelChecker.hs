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

type Answer = (String, Bool)

-- return the model
getModel :: InputModel -> Model
getModel (model, _, _) = model

-- returns the knowledge base considered
getKB :: InputModel -> KnowledgeBase
getKB (_, _, kb) = kb

-- returns the given concepts
getGC :: InputModel -> Givens
getGC (_, gc, _) = gc

-- checks the input model
checkInputModel :: InputModel -> Answer
checkInputModel input = 
  checkModel concepts model `combine`
  (flip checkModel model $ foldl1 (/\) gamma)
  where model    = getModel input
        gamma    = map toNNF $ getKB input
        givens   = map toNNF $ getGC input
        concepts = foldl1 (/\) $ gamma ++ givens
        combine (part1, result1) (part2, result2) =
                ("---- All concepts:\n"++part1++
                 "---- Gamma only:\n"++part2, result1 && result2)

{-
  Laziness will prevent to check the concept for every model
  Once one element validates the model it terminates :)
-}

-- check if model is a model for concept
checkModel :: Concept -> Model -> Answer
checkModel _ ([], _, _ ) = ("Empty model", False)
checkModel concept model =
  answerOr $ map (checkConcept concept model) $ getDomain model
  where answerOr [(msg, result)]    = (msg,result)
        answerOr ((msg,True):rest)  = (msg,True)
        answerOr ((msg,False):rest) = if restResult then (restMsg, restResult)
                                                    else (msg++restMsg, restResult)
          where (restMsg, restResult) = answerOr rest

-- Checks if a concept is valid in the given model
checkConcept :: Concept -> Model -> Individual -> Answer
-- Assuming that in NNF only not atoms occurs but not not concepts
checkConcept (And f1 f2) model distinguished  = 
  (msg1++msg2, result1 && result2)
  where (msg1, result1) = checkConcept f1 model distinguished
        (msg2, result2) = checkConcept f2 model distinguished
checkConcept (Or f1 f2) model distinguished =
  (newMsg, newResult)
  where (msg1, result1) = checkConcept f1 model distinguished
        (msg2, result2) = checkConcept f2 model distinguished
        newResult       = result1 || result2 
        newMsg          = if newResult then "" else msg1++msg2
checkConcept (Exists relation f) model distinguished =
  answerOr $ map (checkConcept f model . snd) elements
  where elements      = filter (matches distinguished) relationSet
        matches ind x = fst x == ind
        relationSet   = fromJust $ lookup relation relations
        relations     = getRelations model
        answerOr :: [Answer] -> Answer
        answerOr [] = (newMsg, False)
          where newMsg = "No successors for relation "++relation++
                         " for "++show distinguished++"\n"
        answerOr ((msg, result) : []) = (msg, result) -- last message 
        answerOr ((msg, True) :rest)  = (msg, True)
        answerOr ((msg, False) :rest) = 
          if restResult then (restMsg, restResult) else (msg++restMsg, restResult) 
          where (restMsg, restResult) = answerOr rest
checkConcept (Forall relation f) model distinguished =
  answerAnd $ map (checkConcept f model . snd) elements
  where elements      = filter (matches distinguished) relationSet
        matches ind x = fst x == ind
        relationSet   = fromJust $ lookup relation relations
        relations     = getRelations model
        answerAnd :: [Answer] -> Answer
        answerAnd [] = ("", True)
        answerAnd ((msg, True) : rest)  = answerAnd rest
        answerAnd ((msg, False) : rest) = (msg,False)
checkConcept any model distinguished =
   -- if all cases fail we have an atom
   checkAtomic any model distinguished 

-- Check if an atomic concept works for a distinguished individual (or no one) in a model
checkAtomic :: Concept -> Model -> Individual -> Answer
checkAtomic _ ([], _, _) _ = ("-Failed since we have an empty domain\n", False)
checkAtomic T _ _ = ("", True)
checkAtomic (Neg T) _ distinguished = (msg, False)
  where msg    = "Failed since we have to satisfy bottom for "++
                 show distinguished++"\n"
checkAtomic (Atom atom) model distinguished = (msg, result)
  where result = isInUnary distinguished atom model
        msg    = if result then "" 
                           else "Failed to satisfy "++atom++
                                " for "++show distinguished++
                                "\n"
checkAtomic (Neg (Atom atom)) model distinguished = (msg, result)
  where result = not $ isInUnary distinguished atom model
        msg    = if result then "" 
                                else "Failed not to satisfy "++atom++
                                     " for "++show distinguished++
                                     "\n"


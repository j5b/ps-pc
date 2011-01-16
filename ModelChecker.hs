{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: checks if a model is valid for a given concepts
-}

module ModelChecker (checkInputModel, checkAtomic, checkConcept, checkGivens) where 

{-
To check if a model is valid
  - 1st : Put all concepts to NNF form
  - 2nd : Choose an individual
  - 3rd : Check the validity for each concept in given model
      - Check atomic formulas one by one
      - Output report for each
  - 4th : Output the report (String, Bool)
-}

import Data.Maybe
import Control.Monad
import Signature
import Model
import Report

type Concepts = [Concept]

type Answer = (String, Bool)

-- Check if the model is a model for gamma and the concepts
checkInputModel :: Model -> Concepts -> Concepts -> Answer
checkInputModel model [] givens = if consistent /= mzero 
                                  then error (consistent ++ printModel model)
                                  else ("---- Model:\n"++show model++
                                        "\n---- All concepts:\n"++msg++
                                        "---- Gamma only:\n", result)        
  where concepts      = foldl1 (/\) $ map toNNF givens
  	(msg, result) = checkGivens concepts model 
        consistent    = consistentModel model
checkInputModel model gamma givens = if consistent /= mzero
                                     then error (consistent ++ printModel model)
                                     else checkGivens concepts model `combine`
                                          checkGamma (foldl1 (/\) $ map toNNF gamma) model
  where concepts = foldl1 (/\) $ map toNNF gamma ++ map toNNF givens
        combine (part1, result1) (part2, result2) =
                ("---- Model:\n"++show model++
                 "\n---- All concepts:\n"++part1++
                 "---- Gamma only:\n"++part2, result1 && result2)
        consistent = consistentModel model

{-
  Laziness will prevent to check a concept for every element of the domain
  Once one element validates the model check for givens terminates :)
  Once one element doesn't validate the model check for gamma terminates :)
-}

{-
  AnswerOr and AnswerAnd occurs in checkGamma/checkGivens and in checkConcept
  although the behavior is very similar it has some differences.
-}

-- Checks if Gamma are satisfied everywhere
checkGamma :: Concept -> Model -> Answer
checkGamma _ ([], _, _ ) = ("Empty model", False)
checkGamma concept model =
  answerAnd $ map (checkConcept concept model) $ getDomain model
  where answerAnd [(msg, result)]     = (msg, result)
        answerAnd ((msg,False):rest)  = (msg,False)
        answerAnd ((msg,True):rest)   = answerAnd rest

-- Checks if Givens are satisfied somewhere
checkGivens :: Concept -> Model -> Answer
checkGivens _ ([], _, _ ) = ("Empty model", False)
checkGivens concept model =
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
  if result1 then (msg2, result2) else (msg1, result1)
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
        relationSet   = fromMaybe [] $ lookup relation relations
        relations     = getRelations model
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
        relationSet   = fromMaybe []  $ lookup relation relations
        relations     = getRelations model
        answerAnd [] = ("", True)
        answerAnd ((msg, True) : rest)  = answerAnd rest
        answerAnd ((msg, False) : rest) = (msg,False)
checkConcept any model distinguished =
   -- if all cases fail we have an atom
   checkAtomic any model distinguished 

-- Check if an atomic concept works for a distinguished individual (or no one) in a model
checkAtomic :: Concept -> Model -> Individual -> Answer
checkAtomic _ ([], _, _) _ = ("Failed since we have an empty domain\n", False)
checkAtomic T _ _ = ("", True)
checkAtomic (Neg T) _ distinguished = (msg, False)
  where msg    = "Failed to satisfy bottom for "++
                 show distinguished++"\n"
checkAtomic (Atom atom) model distinguished = (msg, result)
  where result = isInUnary distinguished atom model
        msg    = if result then "" else "Failed to satisfy "++atom++
                                        " for "++show distinguished++"\n"
checkAtomic (Neg (Atom atom)) model distinguished = (msg, result)
  where result = not $ isInUnary distinguished atom model
        msg    = if result then "" else "Failed to not satisfy "++atom++
                                        " for "++show distinguished++"\n"
checkAtomic other _ _ = error $ "Non exhaustive pattern: "++show other

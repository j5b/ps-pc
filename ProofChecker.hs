{- 
   Author: Ka Wai Cheng
   Maintainer: Ka Wai Cheng
   Email: <mail.kawai.cheng@gmail.com>
   License: GPL 3.0
   File: ProofChecker.hs
   Description: checks given proof tree is correct and shows unsatisfiability
-}

module ProofChecker where

import Proof
import ProofUtils
import Signature
import ProofUtils

import Data.List

{-
  Called by other modules to check a proof tree
  Checks initial set of concepts and gamma before checking the proof
-}
checkProof :: ProofTree -> [Concept] -> (String, Bool)
checkProof prooftree gamma
  | length (nub cs) /= length cs =
      ("Initial concepts must not contain duplicate concepts", False)
  | length (nub gamma) /= glength =
      ("Gamma must not contain duplicate concepts", False)
  | not $ all isNNF cs =
      ("Initial concepts are not in negation normal form", False)
  | not $ all isNNF gamma =
      ("Concepts in gamma are not in negation normal form", False)
  | length (gamma `intersect` cs) /= glength =
      ("Concepts in gamma are not in the initial set of concepts", False)
  | otherwise =
      checkTree prooftree gamma
  where cs = getConcepts prooftree
        glength = length gamma

{-
  Checks a proof tree shows unsatisfiability and returns true if so
  Else returns messages of incorrect steps at point in proof tree & false
  Pre: Gamma and initial set of concepts do not contain duplicates
-}
checkTree :: ProofTree -> [Concept] -> (String, Bool)
checkTree (NodeZero step) gamma
  | not success =
      (msg, False)
  | cs /= [] =
      ("Proof tree is not complete, applying the " ++ rule ++ " rule to {" ++
       showConceptList initialconcepts ++ "} produces {" ++ showResults cs ++
       "}", False)
  | otherwise =
      ("", True)
  where (msg, success, cs) = checkProofStep step gamma
        (initialconcepts, rule, _) = step
checkTree (NodeOne step tree) gamma
--  | not $ rule `elem` ["and", "exists"] =
--      ("Applying the " ++ rule ++ " to {" ++ showConceptList initialconcepts ++
--       "} should not give 1 result", False)
  | not success =
      (msg, False)
  | length cs /= 1 =
      ("There should be 1 resulting set of concepts for NodeOne", False)
  | conceptEquals (head cs) (getConcepts tree) =
      checkTree tree gamma
  | otherwise =
      ("Next step's concepts (" ++ showConceptList (getConcepts tree) ++
       ") do not match the result of applying " ++ rule ++ " rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, cs) = checkProofStep step gamma
        (initialconcepts, rule, _) = step
checkTree (NodeTwo step t1 t2) gamma
  | rule /= "or" =
      ("Applying the " ++ rule ++ " rule to {" ++ showConceptList initialconcepts ++
       "} should not give 2 results", False)
  | not success =
      (msg, False)
  | length cs /= 2 =
      ("There should be 2 resulting sets of concepts for applying Or rule to ("
       ++ showConceptList initialconcepts ++ ")",
       False)
  | conceptEquals (head cs) (getConcepts t1) &&
    conceptEquals (cs !! 1) (getConcepts t2) ||
    conceptEquals (cs !! 1) (getConcepts t1) &&
    conceptEquals (head cs) (getConcepts t2)    =
      (msg1 ++ msg2, tsuccess1 && tsuccess2)
  | otherwise =
      ("Next step's set of concepts (" ++ showConceptList (getConcepts t1)
       ++ ") and (" ++ showConceptList (getConcepts t2) ++
       ") do not match the results of applying the or rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, cs) = checkProofStep step gamma
        (msg1, tsuccess1) = checkTree t1 gamma
        (msg2, tsuccess2) = checkTree t2 gamma
        (initialconcepts, rule, _) = step

{-
  Returns a string of sets of concepts
  WARNING: only accepts a maximum of 2 results
-}
showResults :: [[Concept]] -> String
showResults [] = "Empty"
showResults [cs] = showConceptList cs
showResults [c1,c2] = showConceptList c1 ++ "} and {" ++ showConceptList c2
showResults _ = "There should not be more than 2 results"

{-
  Checks a single proof step, returns true and resulting concepts if correct
  Else returns error message, false and given list of concepts of the step
  Pre:  set of concept in proofstep and gamma contain no duplicates
  Post: lists of concepts returned contain no duplicates
-}
checkProofStep :: ProofStep -> [Concept] -> (String, Bool, [[Concept]])
checkProofStep (cs, "bottom", Atom a) _
  | Atom a `elem` cs && Neg (Atom a) `elem` cs =
      ("", True, [])
  | otherwise =
      (showConcept (Atom a) ++ " and " ++ showConcept (Neg (Atom a)) ++
       " do not both exist in the set of concepts {" ++ showConceptList cs ++
       "}", False, [cs])
checkProofStep (cs, rule, c) gamma
  | c `elem` cs =
      checkRule (cs, rule, c) gamma
  | otherwise   =
      (showConcept c ++ " does not exist in the set of concepts {" ++
       showConceptList cs ++ "}", False, [cs])

{-
  Checks the proof rule can be applied, returns true and resulting concepts if correct
  Else returns error message, false and given list of concepts of the step
  Pre: ProofStep concept exists
-}
checkRule :: ProofStep -> [Concept] -> (String, Bool, [[Concept]])
checkRule (cs, "", Neg T) _ = ("", True, [])
checkRule (cs, "", _) _ =
  ("This proof tree does not show unsatisfiability", False, [cs])
checkRule (cs, "and", And l r) _ = ("", True, [nub $ delete (And l r) (l:r:cs)])
checkRule (cs, "or", Or l r) _ = ("", True, [nub (l:result), nub (r:result)])
  where result = delete (Or l r) cs
checkRule (cs, "exists", Exists r c) gamma = ("", True, [nub $ c:fsuccs ++ gamma])
  where fsuccs = [successor | Forall relation successor <- cs, relation == r]
checkRule (cs, rule, c) _ =
  (rule ++ " rule cannot be applied to " ++ showConcept c ++ "", False, [cs])
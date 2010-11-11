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
import Signature
import Data.List

{-
  Called by other modules to check a proof tree
  Checks initial set of concepts and gamma before checking the proof
-}
checkProof :: ProofTree -> [Concept] -> (String, Bool)
checkProof prooftree gamma
  | length (nub cs) /= length cs =
      error "Initial concepts must not contain duplicate concepts"
  | length (nub gamma) /= glength =
      error "Gamma must not contain duplicate concepts"
  | not $ all isNNF cs =
      error "Concepts are not in negation normal form"
  | not $ all isNNF gamma =
      error "Concepts in gamma are not in negation normal form"
  | length (gamma `intersect` cs) /= glength =
      error "Concepts in gamma are not in the initial set of concepts"
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
checkTree (NodeZero (Neg T)) _ = ("", True)
checkTree (NodeZero _) _ =
  ("This proof tree does not show unsatisfiability", False)
checkTree (NodeOne step tree) gamma
  | not success =
      (msg, False)
  | cs /= [] =
      ("There should be 1 resulting set of concepts for NodeOne", False)
  | conceptEquals c (getConcepts tree) =
      checkTree tree gamma
  | otherwise =
      ("Next step's concepts (" ++ showConceptList (getConcepts tree) ++
       ") do not match the result of applying " ++ rule ++ " rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, c:cs) = checkProofStep step gamma
        (initialconcepts, rule,_) = step
checkTree (NodeTwo step t1 t2) gamma
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
       ") do not match the results of applying the Or rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, cs) = checkProofStep step gamma
        (msg1, tsuccess1) = checkTree t1 gamma
        (msg2, tsuccess2) = checkTree t2 gamma
        (initialconcepts, rule, _) = step

{-
  Checks a single proof step, returns true and resulting concepts if correct
  Else returns error message, false and given list of concepts of the step
  Pre:  set of concept in proofstep and gamma contain no duplicates
  Post: lists of concepts returned contain no duplicates
-}
checkProofStep :: ProofStep -> [Concept] -> (String, Bool, [[Concept]])
checkProofStep (cs, "bottom", Atom a) _
  | Atom a `elem` cs && Neg (Atom a) `elem` cs =
      ("", True, [[Neg T]])
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
checkRule (cs, "and", And l r) _ = ("", True, [nub $ delete (And l r) (l:r:cs)])
checkRule (cs, "or", Or l r) _ = ("", True, [nub (l:result), nub (r:result)])
  where result = delete (Or l r) cs
checkRule (cs, "exists", Exists r c) gamma = ("", True, [nub $ c:fsuccs ++ gamma])
  where fsuccs = [successor | Forall relation successor <- cs, relation == r]
checkRule (cs, rule, c) _ =
  (rule ++ " rule cannot be applied to " ++ showConcept c ++ "", False, [cs])
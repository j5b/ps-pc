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
import List

-- Public function called by other modules to check a proof tree
proofChecker :: ProofTree -> [Concept] -> (String, Bool)
proofChecker prooftree gamma
  | length (nub cs) /= length cs =
      error ("Initial concepts must not contain duplicate concepts")
  | length (nub gamma) /= glength =
      error ("Gamma must not contain duplicate concepts")
  | not (and (map isNNF cs)) =
      error ("Concepts are not in negation normal form")
  | not (and (map isNNF gamma)) =
      error ("Concepts in gamma are not in negation normal form")
  | length (intersect gamma cs) /= glength =
      error ("Concepts in gamma are not in the initial set of concepts")
  | otherwise =
      checkProof prooftree gamma
  where cs = getConcepts prooftree
        glength = length gamma

{-
  Checks a proof tree shows unsatisfiability and returns true if so
  Else returns messages of incorrect steps at point in proof tree & false
  Counts step?
-}
checkProof :: ProofTree -> [Concept] -> (String, Bool)
checkProof (NodeZero (Neg T)) _ = ("", True)
checkProof (NodeZero _) _ =
  ("This proof tree does not show unsatisfiability", False)
checkProof (NodeOne step tree) gamma
  | not success =
      (msg, False)
  | cs /= [] =
      ("There should be 1 resulting set of concepts for NodeOne", False)
  | conceptEquals c (getConcepts tree) =
      checkProof tree gamma
  | otherwise =
      ("Next step's concepts (" ++ showConceptList (getConcepts tree) ++
       ") do not match the result of applying " ++ rule ++ " rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, c:cs) = checkProofStep step gamma
        (initialconcepts, rule,_) = step
checkProof (NodeTwo step t1 t2) gamma
  | not success =
      (msg, False)
  | length cs /= 2 =
      ("There should be 2 resulting sets of concepts for applying Or rule to ("
       ++ showConceptList initialconcepts ++ ")",
       False)
  | conceptEquals (cs !! 0) (getConcepts t1) &&
    conceptEquals (cs !! 1) (getConcepts t2) ||
    conceptEquals (cs !! 1) (getConcepts t1) &&
    conceptEquals (cs !! 0) (getConcepts t2)    =
      (msg1 ++ msg2, tsuccess1 && tsuccess2)
  | otherwise =
      ("Next step's set of concepts (" ++ showConceptList (getConcepts t1)
       ++ ") and (" ++ showConceptList (getConcepts t2) ++
       ") do not match the results of applying the Or rule to (" ++
       showConceptList initialconcepts ++ ")", False)
  where (msg, success, cs) = checkProofStep step gamma
        (msg1, tsuccess1) = checkProof t1 gamma
        (msg2, tsuccess2) = checkProof t2 gamma
        (initialconcepts, rule, _) = step

{-
  Checks a single proof step, returns true and resulting concepts if correct
  Else returns error message, false and given list of concepts of the step
-}
checkProofStep :: ProofStep -> [Concept] -> (String, Bool, [[Concept]])
checkProofStep (cs, "Bottom", Atom a) _
  | elem (Atom a) cs && elem (Neg (Atom a)) cs =
      ("", True, [[Neg T]])
  | otherwise =
      (showConcept (Atom a) ++ " and " ++ showConcept (Neg (Atom a)) ++
       " do not both exist in the set of concepts (" ++ showConceptList cs ++
       ")", False, [cs])
checkProofStep (cs, "And", And l r) _
  | elem (And l r) cs =
      ("", True, [nub ((l:r:cs) \\ [(And l r)])])
  | otherwise =
      (showConcept (And l r) ++ " does not exist in the set of concepts (" ++
       showConceptList cs ++ ")", False, [cs])
checkProofStep (cs, "Or", Or l r) _
  | elem (Or l r) cs =
      ("", True, [nub (l:result), nub (r:result)])
  | otherwise =
      (showConcept (Or l r) ++ " does not exist in the set of concepts (" ++
       showConceptList cs ++ ")", False, [cs])
  where result = delete (Or l r) cs
checkProofStep (cs, "Exists", (Exists r c)) gamma
  | elem (Exists r c) cs =
      ("", True, [nub (c:(fsuccs ++ gamma))])
  | otherwise =
      (showConcept (Exists r c) ++ " does not exist in the set of concepts (" ++
       showConceptList cs ++ ")", False, [cs])
  where fsuccs = [successor | Forall relation successor <- cs, relation == r]
checkProofStep (cs, rule, c) _ =
  (rule ++ " rule cannot be applied to " ++ showConcept c ++ "", False, [cs])
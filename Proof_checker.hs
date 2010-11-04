{- 
   Author: Ka Wai Cheng
   Maintainer: Ka Wai Cheng
   Email: mail.kawai.cheng@gmail.com
   License: GPL 3.0
   File: proof_checker.hs
   Description: checks the steps in a given proof tree are correct
-}

module Proof_checker where

import Proof
import Signature
import List

-- Checks a proof tree, returns messages of incorrect steps & true if proof constructed correctly
-- Also checks that proof shows unsatisfiability, returns true if proof is
-- [concept] = gamma
checkProof :: ProofTree -> [Concept] -> (String, Bool)
checkProof (NodeZero (Neg T)) _ = ("", True)
checkProof (NodeZero _) _ = ("This proof tree does not show unsatisfiability", False)
--checkProof (NodeZero _) _ = ("The leaf of the prooftree is not Neg T", False)
checkProof (NodeOne step tree) g
  | not success                        = (msg, False)
  | fs /= []                           = ("There is more than 1 resulting list of concepts for NodeOne", False )
  | conceptEquals f (getConcepts tree) = checkProof tree g
  | otherwise                          = ("Next steps formulas do not match", False)
    where (msg, success, f:fs) = checkProofStep step g
checkProof (NodeTwo step t1 t2) g
  | not success                          = (msg, False)
  | fs /= []                             = ("There should be 2 resulting lists of conepts for NodeTwo", False)
  | conceptEquals f1 (getConcepts t1) &&
    conceptEquals f2 (getConcepts t2) ||
    conceptEquals f2 (getConcepts t1) &&
    conceptEquals f1 (getConcepts t2)    = (msg1 ++ msg2, tsuccess1 && tsuccess2)
  | otherwise                            = ("Next steps formulas do not match", False)
    where (msg, success, f1:f2:fs) = checkProofStep step g
	  (msg1, tsuccess1) = checkProof t1 g
	  (msg2, tsuccess2) = checkProof t2 g

-- Tests lists of concepts are equivalent in set theory
-- Should not be called with [] anyway
conceptEquals :: [Concept] -> [Concept] -> Bool
conceptEquals [] _ = False
conceptEquals c1 c2 = (c1 \\ c2 == []) && (c2 \\ c1 == [])

-- Returns the list of concepts immediately before rule is applied at root of tree
getConcepts :: ProofTree -> [Concept]
getConcepts (NodeZero c) = [c]
getConcepts (NodeOne (cs, _, _) _) = cs
getConcepts (NodeTwo (cs, _, _) _ _) = cs

-- Checks a single proof step, returns true and resulting list of formulas if the proof step is correct
-- Else returns false and given list of fomulas
-- [concept] = gamma - only applicable in Exists rule
checkProofStep :: ProofStep -> [Concept] -> (String, Bool, [[Concept]])
checkProofStep (fs, "Bottom", Atom a) _
  | elem (Atom a) fs &&
    elem (Neg (Atom a)) fs = ("", True, [[(Neg T)]])
  | otherwise              = ("Atom " ++ a ++ " and Neg Atom " ++ a ++
                              " do not exist", False, [fs])
checkProofStep (fs, "And", And l r) _
  | elem (And l r) fs = ("", True, [l:r:result])
  | otherwise         = ("And " ++ (show l) ++ " " ++ (show r) ++
                         " does not exist", False, [fs])
  where result = delete (And l r) fs
checkProofStep (fs, "Or", Or l r) _
  | elem (Or l r) fs = ("", True, [l:result, r:result])
  | otherwise        = ("Or " ++ show l ++ " " ++ show r ++
                        " does not exist", False, [fs])
  where result = delete (Or l r) fs
checkProofStep (fs, "Exists", (Exists r c)) g
  | elem (Exists r c) fs = ("", True, [c:(g ++ fsuccs)])
  | otherwise            = ("Exist " ++ show r ++ "." ++ show c ++
                            " does not exist", False, [fs])
  where fsuccs = [s | Forall rn s <- fs, rn == r]
checkProofStep (fs, rule, f) _ = (rule ++ " rule cannot be applied to " ++
                                show f, False, [fs])

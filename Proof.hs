{- 
   Author: Michal Gerard Parusinski
   Maintainer: TBD
   Email: TBD
   License: GPL 3.0
   File: Proof.hs
   Description: provides a framework to build and handle description logic proofs
-}

module Proof where 

import Signature
import List

-- TODO: Add somehow knowledge base
-- TODO: Build up the proof rules

-- The last concept in the tuple is the concept that the rule is applied to
type Rule = String
type ProofStep = ([Concept], Rule, Concept)
data ProofTree = NodeZero Concept |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTree ProofTree
               deriving (Show)


-- Functions used in Proof Checker, may or may not be useful in other modules

-- Tests lists of concepts are equivalent in set theory
-- WARNING: comparing empty lists may not give desired result in the calling function
conceptEquals :: [Concept] -> [Concept] -> Bool
conceptEquals c1 c2 = (c1 \\ c2 == []) && (c2 \\ c1 == [])

-- Returns the list of concepts before the rule is applied at root of tree
getConcepts :: ProofTree -> [Concept]
getConcepts (NodeZero c) = [c]
getConcepts (NodeOne (cs, _, _) _) = cs
getConcepts (NodeTwo (cs, _, _) _ _) = cs
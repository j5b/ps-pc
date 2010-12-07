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
import Data.List

-- TODO: Add somehow knowledge base
-- TODO: Build up the proof rules

-- The last concept in the tuple is the concept that the rule is applied to
type Rule = String
type ProofStep = ([Concept], Rule, Concept)
data ProofTree = NodeZero ProofStep |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTree ProofTree
               deriving (Show, Eq)

getConcepts :: ProofStep -> [Concept]
getConcepts (concepts,_,_) = concepts

getRule :: ProofStep -> Rule
getRule (_,rule,_) = rule

getConceptUsed :: ProofStep -> Concept
getConceptUsed (_,_,used) = used
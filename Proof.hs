{- 
   Author: Michal Gerard Parusinski
   Maintainer: TBD
   Email: TBD
   License: GPL 3.0
   File: signature.hs
   Description: provides a framework to build and handle description logic proofs
-}

module Proof where 

import Signature

-- TODO: Add somehow knowledge base
-- TODO: Build up the proof rules

-- The last Formula in the tuple is the concept that the rule is applied to
type Rule = String
type ProofStep = ([Concept], Rule, Concept)
data ProofTree = NodeZero Concept |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTree ProofTree
               deriving (Show)

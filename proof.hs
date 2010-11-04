{- 
   Author: Michal Gerard Parusinski
   Maintainer: TBD
   Email: TBD
   License: GPL 3.0
   File: signature.hs
   Description: provides a framework to build and handle description logic proofs
-}

module Proof where 

-- TODO: Add somehow knowledge base
-- TODO: Build up the proof rules

-- The last Formula in the tuple is the concept that the rule is applied to
type Rule = String
type ProofStep = ([Formula], Rule, Formula)
data ProofTree = NodeZero ProofStep |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTtree ProofTree
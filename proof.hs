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

type Rule = String
type ProofStep = ([Formula], Rule)
data ProofTree = NodeZero ProofStep |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTtree ProofTree |
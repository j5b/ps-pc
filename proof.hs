-- file: proof.hs

module Proof where 

type Rule = String
type ProofStep = ([Formula], Rule)
data ProofTree = NodeZero ProofStep |            
                 NodeOne ProofStep ProofTree |
                 NodeTwo ProofStep ProofTtree ProofTree |
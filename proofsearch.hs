{- 
   Author: Jannis, Saguy
   License: GPL 3.0
   File: proofsearch.hs
   Description: Proof/Model search for description logics
-}

{-
   Here we will write a long description at some point.
-}

import Data.Either
import Model
import Proof
import Signature 

-- despatching function
--findPOM :: [Concept] -> [Concept] -> Either Model Proof
--findPOM cs gamma = findProofOrModel (map toNegNormal conceptSort(cs++gamma)) (map toNegNormal gamma) ([],[],[]) 

toNegNormal = id
conceptSort = id

-- Maps a concept to either a proof or model.
-- pre: Assume sorted for now.
findProofOrModel :: [Concept] -> [Concept] ->  Either Model ProofTree
findProofOrModel [] _                     = Left ([],[],[])
findProofOrModel ((T):cs) gamma        = findProofOrModel cs gamma
findProofOrModel ((Atom c):(Neg (Atom d)):cs) gamma = if  c == d then Right (NodeOne (((Atom c):(Neg (Atom d)):cs), "bottom", (Atom c)) (NodeZero (Neg T)))
                                                        else findProofOrModel cs gamma
findProofOrModel ((And c d):cs) gamma  = either Left (Right . g) (findProofOrModel (conceptSort (c:d:cs)) gamma)
    where
      g pf = NodeOne ((And c d):cs, "and", (And c d)) pf
findProofOrModel ((Or c d):cs) gamma   = either Left g (findProofOrModel (conceptSort (c:cs)) gamma)
    where
      g pf       = either Left (Right . (g' pf)) (findProofOrModel (conceptSort (d:cs)) gamma)
      g' pf1 pf2 = NodeTwo ((Or c d):cs, "or", (Or c d)) pf1 pf2
--findProofOrModel ((Exists rel c):cs) gamma  = g (map (findProofOrModel with the right aprameters) (filteroutexists ((Exists rel c):cs)))
--    where
--      g rs = if null (rights rs) then join (lefts rs) else head rights
--findProofModel cs gamma  = construct a model


-- Function documentation/description (not comments)
--someFunction :: a -> b -> ...
--someFunction = definition

{-
   Always explain functions just above them so Hackage can help us generate documentation
   "TODO:" Will denote a task to do
   "WARNING:" Some important information to put in the documentation 
-}

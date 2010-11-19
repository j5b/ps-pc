{- 
   Author: Jannis
   License: GPL 3.0
   File: ProofUtils.hs
   Description: A collection of useful functions.
-}

module ProofUtils where

import Signature
--
-- Predicates for the Concept types.
--

isTop :: Concept -> Bool
isTop (T) = True
isTop _   = False

isBot :: Concept -> Bool
isBot (Neg T) = True
isBot _       = False

isAtom :: Concept -> Bool
isAtom (Atom _) = True
isAtom _        = False

isNeg :: Concept -> Bool
isNeg (Neg _) = True
isNeg _       = False

isNegAtom :: Concept -> Bool
isNegAtom (Neg (Atom _)) = True
isNegAtom _              = False

isOr :: Concept -> Bool
isOr (Or _ _) = True
isOr _        = False

isAnd :: Concept -> Bool
isAnd (And _ _) = True
isAnd _         = False

isExists :: Concept -> Bool
isExists (Exists _ _) = True
isExists _            = False

isForall :: Concept -> Bool
isForall (Forall _ _) = True
isForall _            = False

--
-- Other functions.
--

-- Interleaves two lists.
interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs     []     = xs
interleave []     ys     = ys

-- rules

bRule = "bottom"
aRule = "and"
oRule = "or"
eRule = "exists"

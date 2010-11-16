{- 
   Author: Jannis, Saguy
   License: GPL 3.0
   File: proofsearch.hs
   Description: Proof/Model search for description logics.
-}

{-
   Here we will write a long description at some point.
-}

module ProofSearch where

import Data.Either
import Data.List

import Model
import Proof
import ProofUtils
import Signature

-- Takes a knowledge base and a set of concepts and returns either a proof
-- showing inconsistency or a model.
findPOM :: [Concept] -> [Concept] -> Either Model ProofTree
findPOM cs gamma = either (Left . fst) Right
                   (findProofOrModel (conceptSort $ map toNNF cs++gamma)
                    (map toNNF gamma) [1..])

-- Maps a set of concepts to either a proof or model.
findProofOrModel :: [Concept] -> [Concept] -> [Individual]
                    -> Either (Model, [Individual]) ProofTree
findProofOrModel [] _ (i:is)
  = Left (([i],[],[]), is)
findProofOrModel (T:cs) gamma is
  = findProofOrModel cs gamma is
findProofOrModel (Neg T:cs) gamma is
  = Right (NodeZero (Neg T : cs, "", Neg T))
findProofOrModel (Atom c : Neg (Atom d) : cs) _ _
  = if  c == d 
    then Right (NodeZero (Atom c : Neg (Atom d) : cs, "bottom", Atom c))
    else error "Can't deal with concepts in wrong order."
findProofOrModel (And c d : cs) gamma is 
  = either Left (Right . g) (findProofOrModel (conceptSort (c:d:cs)) gamma is)
    where
      g = NodeOne (And c d : cs, "and", And c d)
findProofOrModel (Or c d : cs) gamma is
  = either Left g (findProofOrModel (conceptSort (c:cs)) gamma is)
    where
      g pf = either Left (Right . g' pf)
             (findProofOrModel (conceptSort (d:cs)) gamma is)
      g' = NodeTwo (Or c d : cs, "or", Or c d)
findProofOrModel (Exists rel c : cs) gamma is
  = foldExists (filter isExists (Exists rel c : cs)) is
    where
      foldExists :: [Concept] -> [Individual]
                    -> Either (Model, [Individual]) ProofTree
      foldExists [] is = Left (([], [], []), is)
      foldExists (Exists rel c : es) (i:is)
        = either f (Right . g) (findProofOrModel
                                (applyExists cs gamma (Exists rel c)) gamma is)
          where
            g = NodeOne (Exists rel c : cs, "exists", Exists rel c)
            f (m, is') = either (\(m', is'') -> Left (joinModels m' m''', is''))
                         (Right . g) (foldExists es (i : is'))
              where
                m''  = joinModels ([i], [], [(rel, [(i, head is)])]) m
                m''' = joinModels (constructAtomicModel cs i) m''
findProofOrModel cs gamma (i:is) = Left (constructAtomicModel cs i, is)

-- A function that sorts concepts in the following order, first to last:
-- 'A, not A', 'A and B', 'A or B', 'ER.C', others
conceptSort :: [Concept] -> [Concept]
conceptSort = putContradictionsFirst . sortBy compareConcepts
      where
        compareConcepts :: Concept -> Concept -> Ordering
        compareConcepts (And _ _) _    = LT
        compareConcepts _ (And _ _)    = GT
        compareConcepts (Or _ _) _     = LT
        compareConcepts _ (Or _ _)     = GT
        compareConcepts (Exists _ _) _ = LT
        compareConcepts _ (Exists _ _) = GT
        compareConcepts _ _            = EQ
        
        putContradictionsFirst :: [Concept] -> [Concept]
        putContradictionsFirst cs = contradictions ++ (cs \\ contradictions)
          where
            contradictions = interleave cas $ map Neg cas
            cas            = as `intersect` nas
            as             = filter isAtom cs
            nas            = map (\(Neg a) -> a) $ filter isNegAtom cs

-- Joins two models.
joinModels :: Model -> Model -> Model
joinModels (dom, unaryR, binaryR) (dom', unaryR', binaryR')
  = (nub $ dom++dom', joinRels unaryR unaryR', joinRels binaryR binaryR')
    where
      joinRels :: Eq a => [(String, [a])] -> [(String, [a])] -> [(String, [a])]
      joinRels [] rs'
        = rs'
      joinRels ((name, set) : rs) rs'
        = (name, maybe set (nub . (++set)) $ lookup name rs')
          : joinRels rs (filter ((/=name) . fst) rs')

-- Constructs a model from the atomic formulas in the set of given concepts.
constructAtomicModel :: [Concept] -> Individual -> Model
constructAtomicModel cs i = ([i], unaryRelations, [])
  where
    unaryRelations = map (\(Atom a) -> (a, [i])) $ filter isAtom cs

-- Takes the knowledge base, a set of concepts and a there exists formula and
-- maps them to concepts according to the Exists tableaux rule.
applyExists :: [Concept] -> [Concept] -> Concept -> [Concept]
applyExists cs gamma (Exists rel c)
  = conceptSort . nub $ c : gamma ++ 
    map (\(Forall _ c) -> c) (filter (isMatchingForall rel) cs)
  where
    -- Checks if the given concept is is a forall with the specified relation.
    isMatchingForall :: String -> Concept -> Bool
    isMatchingForall rel' (Forall rel c) = rel == rel'
    isMatchingForall _ _                 = False

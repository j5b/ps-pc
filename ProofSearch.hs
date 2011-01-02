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
import Data.Maybe

import Model
import Proof
import ProofUtils
import Signature

type CacheEntry = Either Model ProofTree
type Cache      = [([Concept], (Individual, CacheEntry))]

-- Takes a knowledge base and a set of concepts and returns either a proof
-- showing inconsistency or a model.
findPOM :: [Concept] -> [Concept] -> Either Model ProofTree
findPOM cs gamma = either (Left . sortModel . fst) Right $ fst $
                   findProofOrModel (conceptSort . nub $ map toNNF cs++gamma)
                   (nub $ map toNNF gamma) [1..] []

-- Maps a set of concepts to either a proof or model. At this stage only
-- the caching is handled.
findProofOrModel :: [Concept] -> [Concept] -> [Individual] -> Cache
                    -> (Either (Model, [Individual]) ProofTree, Cache)
findProofOrModel cs gamma (i:is) cache
  = maybe (result, cache') fromCache $ lookup cs cache
    where
      expanding          = (cs, (i, Left ([],[],[])))
      result             = findProofOrModel' cs gamma (i:is) (expanding:cache)
      resulttocash       = either (Left . fst) Right result
      cache'             = (cs, (i, resulttocash)):cache
      fromCache (_, pom) = (either (Left . (\m -> (m, i:is))) Right pom, cache)

-- Maps a set of concepts to either a proof or model. Here is the actual
-- logic of the function.
findProofOrModel' :: [Concept] -> [Concept] -> [Individual] -> Cache
                    -> Either (Model, [Individual]) ProofTree
findProofOrModel' [] _ (i:is) _
  = Left (([i],[],[]), is)
findProofOrModel' (T : cs) _ (i:is) _
  = Left (([i],[],[]), is)
findProofOrModel' (Neg T:cs) _ _ _
  = Right (NodeZero (Neg T : cs, "", Neg T))
findProofOrModel' (Atom c : Neg (Atom d) : cs) _ (i:is) _
  = if  c == d
    then Right (NodeZero (Atom c : Neg (Atom d) : cs, "bottom", Atom c))
    else Left (constructAtomicModel (Atom c : cs) i, is)
findProofOrModel' (And c d : cs) gamma is cache
  = either Left (Right . constructProof)
    (fst $ findProofOrModel cs' gamma is cache)
    where
      constructProof = NodeOne (And c d : cs, "and", And c d)
      cs' = conceptSort $ c `uniqueCons` (d `uniqueCons` cs)
findProofOrModel' (Or c d : cs) gamma is cache
  = either Left constructProofOrFindModel pom
    where
      (pom, cache')
          = findProofOrModel (conceptSort $ c `uniqueCons` cs) gamma is cache
      constructProofOrFindModel pf
          = either Left (Right . constructProof pf) $ fst $
            findProofOrModel (conceptSort $ d `uniqueCons` cs) gamma is cache'
      constructProof = NodeTwo (Or c d : cs, "or", Or c d)
findProofOrModel' (Exists rel c : cs) gamma is cache
  = foldExists (Exists rel c : cs) gamma (filter isExists (Exists rel c : cs)) is cache
findProofOrModel' cs gamma (i:is) _ = Left (constructAtomicModel cs i, is)

-- Deals with the exists case by doing the following:
--   (i)  If a proof is found, it is immediately returned.
--   (ii) If a model is found, it recurses on the following exist concepts and
--        merges the models if all model constructions were succesful, otherwise
--        a proof is returned.
foldExists :: [Concept] -> [Concept] -> [Concept] -> [Individual] -> Cache
              -> Either (Model, [Individual]) ProofTree
foldExists cs _ [] (i:is) _
  = Left (constructAtomicModel cs i, is)
foldExists cs gamma (Exists rel c : es) (i:is) cache
  = either dealWithModel (Right . constructProof) pom
  where
    (pom, cache') = findProofOrModel cs' gamma is cache
    cs' = applyExists cs gamma (Exists rel c)
    constructProof = NodeOne (cs, "exists", Exists rel c)
    dealWithModel (m, is') = either (\(m'', is'') -> Left (joinModels m' m'', is''))
                             Right (foldExists cs gamma es (i : is') cache')
      where
        m' = joinModels m edgemodel
        edgemodel = maybe forwardEdge createBackEdge $ lookup cs' cache
        forwardEdge = if (head is) `elem` dom then ([i], [], [(rel, [(i, head is)])])
                                              else ([i], [], [(rel, [(i, i)])])
        (dom, _, _) = (fst . fromLeft) pom
        createBackEdge (j, _) = ([i], [], [(rel, [(i, j)])])

-- A function that sorts concepts in the following order, first to last:
-- 'A, not A', 'A and B', 'A or B', 'ER.C', others
conceptSort :: [Concept] -> [Concept]
conceptSort = putFalsityFirst . putContradictionsFirst . sortBy compareConcepts
      where
        compareConcepts :: Concept -> Concept -> Ordering
        compareConcepts (T) _          = GT
        compareConcepts _ (T)          = LT
        compareConcepts (And _ _) _    = LT
        compareConcepts _ (And _ _)    = GT
        compareConcepts (Or _ _) _     = LT
        compareConcepts _ (Or _ _)     = GT
        compareConcepts (Exists _ _) _ = LT
        compareConcepts _ (Exists _ _) = GT
        compareConcepts _ _            = EQ

        putFalsityFirst :: [Concept] -> [Concept]
        putFalsityFirst concepts = falsities ++ (concepts \\ falsities)
          where
            falsities = filter isBot concepts
        
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

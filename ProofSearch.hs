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

-- Either: Individual if unexpanded, ProofOrModel otherwise
type Memory = [([Concept] , Either (Either (Model, [Individual]) ProofTree) Individual)]

-- Takes a knowledge base and a set of concepts and returns either a proof
-- showing inconsistency or a model.
findPOM :: [Concept] -> [Concept] -> Either Model ProofTree
findPOM cs gamma = either (Left . fst) Right
                   (fst $ findProofOrModel (conceptSort . nub $ map toNNF cs++gamma)
                    (nub $ map toNNF gamma) [1..] [])

-- Maps a set of concepts to either a proof or model.
findProofOrModel :: [Concept] -> [Concept] -> [Individual] -> Memory
                    -> (Either (Model, [Individual]) ProofTree, Memory)
-- do we want cashing is base cases?! look at sorting
findProofOrModel [] _ (i:is) memory
  = (Left (([i],[],[]), is), memory)
findProofOrModel (T : cs) _ (i:is) memory
  = (Left (([i],[],[]), is), memory)
findProofOrModel (Neg T:cs) _ is memory
  = (Right (NodeZero (Neg T : cs, "", Neg T)), memory)
findProofOrModel (Atom c : Neg (Atom d) : cs) _ (i:is) memory
  = if  c == d
    then (Right (NodeZero (Atom c : Neg (Atom d) : cs, "bottom", Atom c)), memory)
    else (Left (constructAtomicModel (Atom c : cs) i, is), memory)
findProofOrModel (And c d : cs) gamma is memory
  = (result, newmem) 
    where
      newmem 
        = if findInMemory == [] 
          then (And c d:cs, Left result):newmem' 
          else newmem'
      (result, newmem') 
        = if findInMemory == []
          then (either Left (Right . g) proofOrModel, newmem'')
          else (fromLeft $ snd (head findInMemory), memory)
            where 
               (proofOrModel, newmem'') = findProofOrModel newcs gamma is memory
               g = NodeOne (And c d : cs, "and", And c d)
               newcs = conceptSort $ c `uniqueCons` (d `uniqueCons` cs)
      findInMemory = filter ((==(And c d : cs)) . fst) memory
findProofOrModel (Or c d : cs) gamma is memory
  = (result, newmem)
    where
      newmem 
        = if findInMemory == [] 
          then (Or c d : cs, Left result):newmem' 
          else newmem'
      (result, newmem')
        = if findInMemory == []
          then (either (fst . f) (fst . g) proofOrModel, 
               either (snd . f) (snd . g) proofOrModel)
          else (fromLeft $ snd (head findInMemory), memory)
            where
               (proofOrModel, newmem'') = findProofOrModel 
                                          (conceptSort $ c `uniqueCons` cs) 
                                          gamma is memory
               (proofOrModel2, newmem''') = findProofOrModel 
                                            (conceptSort $ d `uniqueCons` cs) 
                                            gamma is newmem''
               f proof = (Left proof, newmem'')
               g pf = (either Left (Right . g' pf) proofOrModel2, newmem''')
               g' = NodeTwo (Or c d : cs, "or", Or c d)
      findInMemory = filter ((==(Or c d : cs)) . fst) memory
findProofOrModel (Exists rel c : cs) gamma is memory
  = (result, newmem)
      where
        newmem = if findInMemory == []
                 then (Exists rel c : cs, Left result):newmem'
                 else newmem' 
        (result, newmem') 
          | findInMemory == [] =
            foldExists (Exists rel c : cs) gamma
                (filter isExists (Exists rel c : cs))
                is memory
          | isRight $ snd $ head findInMemory =
            createLoopModel (Exists rel c : cs) gamma -- (filter isExists (Exists rel c : cs)) 
                (fromRight $ snd $ head findInMemory)
                is memory
          | otherwise = (fromLeft $ snd (head findInMemory), memory)
        findInMemory = filter ((==(Exists rel c : cs)) . fst) memory

findProofOrModel cs gamma (i:is) memory = (Left (constructAtomicModel cs i, is), memory)

-- Deals with the exists case by doing the following:
--   (i)  If a proof is found, it is immediately returned.
--   (ii) If a model is found, it recurses on the following exist concepts and
--        merges the models if all model constructions were succesful, otherwise
--        a proof is returned.
foldExists :: [Concept] -> [Concept] -> [Concept] -> [Individual] -> Memory
              -> (Either (Model, [Individual]) ProofTree, Memory)
foldExists _ _ [] is memory
  = (Left (([], [], []), is), memory)
foldExists cs gamma (Exists rel c : es) (i:is) memory
  = (either (fst . dealWithModel) (Right . fst . constructProof) proofOrModel, 
     either (snd . dealWithModel) (snd . constructProof) proofOrModel)
  where
    (proofOrModel, newmemory) 
        = findProofOrModel (applyExists cs gamma (Exists rel c)) 
                            gamma is ((cs, Right i):memory)
    constructProof pf 
        = (NodeOne (cs, "exists", Exists rel c) pf, newmemory)
    dealWithModel (m, is') 
        = (either (\(m', is'') -> Left (joinModels m' m'', is''))
            Right proofOrModel', newmemory')
      where
        (proofOrModel', newmemory') 
            = foldExists cs gamma es (i : is') newmemory -- findProofOrModel es gamma is' newmem'
        m'' = joinModels m $ joinModels ([i], [], [(rel, [(i, head is)])])
                                        (constructAtomicModel cs i)

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

-- Constructs a model when a loop exists
-- Should have all exists sotred here, need to call proofOrModel for any further exists
createLoopModel :: [Concept] -> [Concept] -> Individual -> [Individual] -> Memory
                   -> (Either (Model, [Individual]) ProofTree, Memory)
createLoopModel (Exists rel c : cs) gamma n (i:is) memory 
   = (either (Left . g) Right proofOrModel, newmem) 
    where
       newmem = (Exists rel c : cs, Left (Left (newmodel, is))):newmem'
       newmem' = filter isNotExists newmem''
         where 
            isNotExists =  (/=(Exists rel c : cs)) . fst
       (proofOrModel, newmem'') = findProofOrModel cs gamma (i:is) memory       
       g (model, indivs) = (joinModels (constructAtomicModel cs i) (m model), indivs)
       m model = joinModels model newmodel
       newmodel = ([i], [], [(rel, [(i, n)])]) -- Pred here!
createLoopModel _ _ _ _ _ = error "createLoopModel is called with wrong params: probably with no exists"

{-createLoopModel :: [Concept] -> [Concept] -> [Concept] -> 
                     Individual -> [Individual] -> Memory -> 
                     (Either (Model, [Individual]) ProofTree, Memory)
createLoopModel (Exists rel c : cs) (Exists rel' c' : es) gamma n (i:is) memory 
   = (result, newmem)
    where
       result = f (newmodel, (i:is))
       newmem = (((Exists rel c : cs), (Left result)):(newmem'))
       newmem' = filter isNotExists newmem''
       isNotExists =  (/=(Exists rel c : cs)) . fst
       newmodel = ([i], [], [(rel, [(i, n)])]) -- Pred here!
       f (m, is') = either (\(m', is'') -> Left (joinModels m' m''', is''))
                         (Right . g) proofOrModel
       g proof = NodeOne (Exists rel c : cs, "exists", Exists rel c) proof
       (proofOrModel, newmem'') = findProofOrModel es gamma (i:is) memory -- (Left (([], [], []), is), newmem') -- foldExists es is' newmem'
       m''  = joinModels ([i], [], [(rel, [(i, head is)])]) newmodel
       m''' = joinModels (constructAtomicModel cs i) m''
-}     
    

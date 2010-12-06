{- 
   Author: Saguy Benaim
   License: GPL 3.0
   Description: test proof searching
-}

module ProofSearch_test where

import ProofSearch
import Signature
import TestUtils
import Model

import Test.HUnit

allpomutests = do putStrLn "==== Testing utility function for proof and model searcher"
                  runTestTT conceptSortTests
                  runTestTT constructAtomicModelTests
                  runTestTT joinModelTests

conceptSortTests = maplabel "Sorting" [testConceptSortEmpty, testConceptSortList, 
                                  testConceptSortContradictions, 
                                  testConceptSortNotSorting]

joinModelTests = maplabel "Joining models" [testJoinModelsEmpty, testJoinModelsBase1,
                                       testJoinModelsBase2, testJoinModelsSimple12,
                                       testJoinModelsSimple21, testJoinModelsSimple13,
                                       testJoinModelsSimple24, testJoinModelsSimple34,
                                       testJoinModelsComplex1, testJoinModelsComplex2,
                                       testJoinModelsComplex3, testJoinModelsComplex4,
                                       testJoinModelsComplex5]
                           

constructAtomicModelTests = 
     maplabel "Constructing atomic models" [testConstructAtomicModelEmpty, 
                                            testConstructAtomicModelSimple, 
                                            testConstructAtomicModelBigger]

allmodelgentests = do putStrLn "==== Testing the proof and model search results directly"
                      runTestTT simpleModelTests
                      runTestTT moreModelTests
                      runTestTT simpleModelTestsKnowledgeBase
                      runTestTT morecomplexModelTests

simpleModelTests = maplabel "Finding simple models with POM Searcher" [testEmptySet, testNegAtom,
                                          testAndAtom, testOrAtom, testExistsAtom, testForallAtom]

moreModelTests = maplabel "Finding more models with POM Searcher" [testMoreExists1, testMoreExists2,
                             testMoreExists3, testMoreExists4, testMoreExists5, testMoreForall1,
			     testMore1, testMore2]

simpleModelTestsKnowledgeBase = 
    maplabel "Finding models for non empty knowledge base with POM Searcher" [testEmptySet', testNegAtom', testAndAtom', testOrAtom',
			     testForallAtom'] --testExistsAtom',

morecomplexModelTests = maplabel "Finding complex models with POM Searcher" 
                                 [testMoreExists1', testMoreExists2', 
                                 testMoreExists3', testMoreExists4']

------------------------------------- Tests for individual functions -------------------------------------------------------

-- conceptSort tests

testConceptSortEmpty = testequality "Failed to sort empty set" [] (conceptSort []) "[]"

testConceptSortList = testequality msg target result (show input)
  where msg = "Failed to sort  some random list, but with no contradictions" 
        input = [forall_r_c, a_and_b, notatoma, exists_r_b, exists_r_c, orExists, atomc, atomb, exists_r_a, forall_r_a, orforalls]
        target = [a_and_b, orExists, orforalls,  exists_r_b, exists_r_c, exists_r_a, forall_r_c, notatoma, atomc, atomb, forall_r_a]
        result = conceptSort input

-- if duplicate in contradictions, do we just take one pair?
testConceptSortContradictions = testequality msg target result $ show input
  where msg = "Failed to sort random list with contradictions"
        input = [forall_r_c, a_and_b, notatomc, exists_r_b, exists_r_c, 
           orExists, atomc, atomb, exists_r_a, notatomc, forall_r_a, notatomc, notatomb, notatomb, orforalls]  
        target = [atomc, notatomc, atomb, notatomb, a_and_b, orExists, orforalls,  exists_r_b, exists_r_c, 
          exists_r_a, forall_r_c, notatomc, forall_r_a, notatomc, notatomb ]
        result = conceptSort input
 
testConceptSortNotSorting = testequality msg target result $ show input
  where msg = "Failed to sort list with negation of atoms"
        input = [forall_r_a, a_or_b, Neg a_or_b]
        target = [a_or_b, forall_r_a, Neg a_or_b]
        result = conceptSort input

-- joinModels tests

simpleModel1 = sortModel ([1,2], [("A",[2])], [("R",[(1,2)])])
simpleModel2 = sortModel ([1,3], [("A",[3])], [("R",[(1,3)])])
simpleModel3 = sortModel ([1,4], [("A",[4])], [("R",[(1,4)])])
simpleModel4 = sortModel ([1,2], [("B",[2])], [("R",[(1,2)])])
model1 = sortModel ([1,2,3,4,6], [("B",[1, 3]), ("A",[4, 6]), ("C",[2]), ("C",[3])], [("R",[(1,2), (1, 3)]), ("S",[(1,4), (2, 6)])])
model2 = sortModel ([1,4,5,6,7,9,10], [("B",[1, 7]), ("A",[1, 7, 8]), ("C",[4, 10]), ("C",[4])], [("R",[(10,2), (1, 10)]), ("S",[(1,5), (5, 6)])])

testJoinModelsEmpty = 
  testequality "Failed to join empty models" emptyModel (joinModels emptyModel emptyModel) input 
    where input = printModel emptyModel++"\n\t\tand\n"++printModel emptyModel 

testJoinModelsBase1 = 
  testequality "Failed to join simple models 1" (sortModel simpleModel2) (sortModel $ joinModels simpleModel2 emptyModel) input
    where input = printModel simpleModel2++"\n\t\tand\n"++printModel emptyModel

testJoinModelsBase2 = 
  testequality "Failed to join simple models 2" (sortModel model1) (sortModel $ joinModels emptyModel model1) input
    where input = printModel emptyModel++"\n\t\tand\n"++printModel model1

-- Simple with simple: should we apply sorting to individuals, tuples to get unique nomether order of calling?
-- Also can it ever be that "C" is defined twice as below within a model?
testJoinModelsSimple12 = 
  testequality "Failed to join simple models 3" target (sortModel $ joinModels simpleModel1 simpleModel2) input
    where target = sortModel ([1,2,3],[("A",[3,2])],[("R",[(1,3),(1,2)])])
          input = printModel simpleModel1++"\n\t\tand\n"++printModel simpleModel2

testJoinModelsSimple21 = 
  testequality "Failed to join simple models 4" target (sortModel $ joinModels simpleModel2 simpleModel1) input
    where target = sortModel ([1,3,2],[("A",[2,3])],[("R",[(1,2),(1,3)])])
          input = printModel simpleModel2++"\n\t\tand\n"++printModel simpleModel1

testJoinModelsSimple13 = 
  testequality "Failed to join simple models 5" target (sortModel $ joinModels simpleModel1 simpleModel3) input
    where target = sortModel ([1,2,4],[("A",[4,2])],[("R",[(1,4),(1,2)])])
          input = printModel simpleModel1++"\n\t\tand\n"++printModel simpleModel2

testJoinModelsSimple24 = 
  testequality "Failed to join simple models 6" target (sortModel $ joinModels simpleModel2 simpleModel4) input
    where target = sortModel ([1,3,2],[("A",[3]),("B",[2])],[("R",[(1,2),(1,3)])])
          input = printModel simpleModel2++"\n\t\tand\n"++printModel simpleModel4

testJoinModelsSimple34 = 
  testequality "Failed to join simple models 7" target (sortModel $ joinModels simpleModel3 simpleModel4) input
    where target = sortModel ([1,4,2],[("A",[4]),("B",[2])],[("R",[(1,2),(1,4)])])
          input = printModel simpleModel3++"\n\t\tand\n"++printModel simpleModel4

testJoinModelsComplex1 = 
  testequality "Failed to join complex models 8" target (sortModel $ joinModels simpleModel1 model1) input
    where target = sortModel ([1,2,3,4,6],[("A",[4,6,2]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3)]),("S",[(1,4),(2,6)])])
          input = printModel simpleModel1++"\n\t\tand\n"++printModel model1

testJoinModelsComplex2 = 
  testequality "Failed to join complex models 9" target (sortModel $ joinModels simpleModel3 model1) input
    where target = sortModel ([1,4,2,3,6],[("A",[4,6]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3),(1,4)]),("S",[(1,4),(2,6)])])
          input = printModel simpleModel3++"\n\t\tand\n"++printModel model1

testJoinModelsComplex3 = 
  testequality "Failed to join complex models 10" target (sortModel $ joinModels simpleModel2 model2) input
    where target = sortModel ([1,3,4,5,6,7,9,10],[("A",[1,7,8,3]),("B",[1,7]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,3)]),("S",[(1,5),(5,6)])])
          input = printModel simpleModel2++"\n\t\tand\n"++printModel model2

testJoinModelsComplex4 = 
  testequality "Failed to join complex models 11" target (sortModel $ joinModels simpleModel4 model2) input
    where target = sortModel ([1,2,4,5,6,7,9,10],[("B",[1,7,2]),("A",[1,7,8]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,2)]),("S",[(1,5),(5,6)])])
          input = printModel simpleModel4++"\n\t\tand\n"++printModel model2

testJoinModelsComplex5 = 
  testequality "Failed to join complex models 12" target (sortModel $ joinModels model1 model2) input
    where target = sortModel ([1,2,3,4,6,5,7,9,10],[("B",[1,7,3]),("A",[1,7,8,4,6]),("C",[4,10,2]),("C",[3])],[("R",[(10,2),(1,10),(1,2),(1,3)]),("S",[(1,5),(5,6),(1,4),(2,6)])])
          input = printModel model1++"\n\t\tand\n"++printModel model2

-- constructAtomicModel

testConstructAtomicModelEmpty = 
  testequality msg ([2], [], []) (constructAtomicModel [] 2) input
    where msg = "Failed to construct an atomic model containing 2 satisfying no concepts"
          input = "[]"++" "++show 2

testConstructAtomicModelSimple = 
  testequality msg ([10],[("A",[10]),("C",[10])],[]) (constructAtomicModel [atoma, atomc, a_or_b] 10) input
    where msg = "Failed to construct an atomic model containing 10 satisfying various but simple concepts"
          input = show [atoma,atomc, a_or_b]++" "++show 10

testConstructAtomicModelBigger = 
  testequality msg ([0],[("A",[0]),("C",[0]),("C",[0]),("B",[0])],[]) result inputString
    where msg = "Failed to construct an atomic model containing 10 satisfying various and complex concepts"
          result = constructAtomicModel [atoma, atomc, exists_r_a, atomc, notatomc, atomb, notatomb, a_and_b, orExists] 0
          inputString = show [atoma, atomc, exists_r_a, atomc, notatomc, atomb, notatomb, a_and_b, orExists]++" "++show 0

-- applyExists

prints :: [Concept] -> [Concept] -> Concept -> String
prints c1 c2 c3 = show c1++" "++show c2++" "++show c3

testApplyExistsEmpty = 
  testequality msg [atoma] (applyExists [] [] exists_r_a) $ prints [] [] exists_r_a
    where msg = "Failed to apply exists rule with no concepts"

testApplyExistsSimple = 
  testequality msg [atoma,atomb,atomc] (applyExists [forall_r_a, forall_r_b, a_or_b, exists_r_c, forall_r_c] [] exists_r_a) input
    where msg = "Failed to apply exists rule with a few forall concepts"
          input = prints [forall_r_a, forall_r_b, a_or_b, exists_r_c, forall_r_c] [] exists_r_a

testApplyExistsOther1 = 
  testequality msg [top, bottom] (applyExists [forall_r_bottom] [] exists_r_top) input
    where msg = "Failed to apply exists rule with one forall concept p1"
          input = prints [forall_r_bottom] [] exists_r_top

testApplyExistsOther2 = 
  testequality msg [bottom, top] (applyExists [forall_r_top] [] exists_r_bottom) input
    where msg = "Failed to apply exists rule with one forall concept p2" 
          input = prints [forall_r_top] [] exists_r_bottom

testApplyExistsOther3 = 
  testequality msg [atoma, forall_r_b, atomc, exists "R" atomd] result input
    where msg = "Failed to apply exists rule with non empty gamma and a few concepts"
          result = applyExists [forall_r_c, exists "R" atomd ] [forall_r_b] exists_r_a
          input = prints [forall_r_c, exists "R" atomd ] [forall_r_b] exists_r_a

------------------------------------- Tests of findPOM for models ----------------------------------------------------------------

----------- tests with empty knowlege base

-- base casses

testEmptySet = testequality msg (Left ([1], [], [])) (findPOM [] []) (printCG [] [])
  where msg = "Failed to find model for empty gamma and no concepts"

testNegAtom = testequality msg (Left ([1], [], [])) (findPOM [notatoma] []) (printCG [notatoma] [])
  where msg = "Failed to find a model for the negation of an atom"

testAndAtom = testequality msg (Left ([1],[("A",[1]),("B",[1])],[])) (findPOM [a_and_b] []) (printCG [a_and_b] [])
  where msg = "Failed to find a model for a conjunction"

testOrAtom  = testequality msg (Left ([1],[("A",[1])],[])) (findPOM [a_or_b] []) (printCG [a_or_b] [])
  where msg = "Failed to find a model for a disjunction"

testExistsAtom = testequality msg (Left ([1,2],[("A",[2])],[("R",[(1,2)])])) (findPOM [exists_r_a] []) (printCG [exists_r_a] [])
  where msg = "Failed to find a model for existential concept"

testForallAtom = testequality msg (Left ([1],[],[])) (findPOM [forall_r_a] []) (printCG [forall_r_a] [])
  where msg = "Failed to find a model for universal concept"

-- test falsity here

testMoreExists1 = testequality msg target (findPOM [atoma, exists_r_a] []) (printCG [atoma, exists_r_a] [])
  where msg = "Failed to find a model for existential concept and an atom" 
        target = Left $ sortModel ([1,2],[("A",[2,1])],[("R",[(1,2)])])

testMoreExists2 = testequality msg target (findPOM [atoma, exists_r_a, exists_r_b] []) (printCG [atoma, exists_r_a, exists_r_b] [])
  where msg = "Failed to find a model for two existential concepts and an atom"
        target = Left $ sortModel ([1,3,2],[("A",[2,1]),("B",[3])],[("R",[(1,2),(1,3)])])

testMoreExists3 = testequality msg target (findPOM [a_or_b, exists_r_b] []) (printCG [a_or_b, exists_r_b] [])
  where msg = "Failed to find a model for existential concept and a disjunction"
        target = Left $ sortModel ([1,2],[("A",[1]),("B",[2])],[("R",[(1,2)])])

testMoreExists4 = testequality msg target (findPOM [a_or_b, atomb, exists_r_b] []) (printCG [a_or_b, atomb, exists_r_b] [])
  where msg = "Failed to find a model for existential, disjunction and an atom 1"
        target = Left $ sortModel ([1,2],[("A",[1]),("B",[2,1])],[("R",[(1,2)])])

testMoreExists5 = testequality msg target (findPOM [atoma, a_or_b, exists_r_b] []) (printCG [atoma, a_or_b, exists_r_b] [])
  where msg = "Failed to find a model for existential, disjunction and an atom 2"
        target= Left $ sortModel ([1,2],[("A",[1]), ("B",[2])],[("R",[(1,2)])])

testMoreForall1 = testequality msg target (findPOM [atoma, exists_r_a, forall_r_b] []) (printCG [atoma, exists_r_a, forall_r_b] [])
  where msg = "Failed to find a model for existential, univeral, and atom"
        target =Left $ sortModel ([1,2],[("A",[2,1]),("B",[2])],[("R",[(1,2)])])

testMore1 = testequality msg target result (printCG concepts [])
  where msg = "Failed to find a model for multiple universals, multiple existentials, and duplicate atomics"
        target = Left $ sortModel ([1,2],[("A",[2,1]),("B",[2])],[("R",[(1,2)])])
        result = findPOM concepts []
        concepts = [atoma, exists_r_a, forall_r_b, exists_r_a, atoma, forall_r_b, atoma, atoma, atoma, forall_r_b, exists_r_a]

testMore2 = testequality msg target result (printCG [atoma, exists_r_a, exists_r_b, forall_r_c] [])
  where msg = "Failed to find a model for univeral, multiple existentials, and atoms"
        target = Left $ sortModel ([1,3,2],[("A",[2,1]),("B",[3]),("C",[2,3])],[("R",[(1,2),(1,3)])])
        result = findPOM [atoma, exists_r_a, exists_r_b, forall_r_c] []

-- some more complex ones

testMoreExists1' = testequality msg target result (printCG [andExists, andforalls] [])
  where msg = "Failed to find a model for conjunction of existentials and conjuctions of universals"
        target = Left $ sortModel ([1,3,2],[("B",[2,3]),("A",[2,3])],[("R",[(1,2),(1,3)])])
        result = findPOM [andExists, andforalls] []

testMoreExists2' = testequality msg target result (printCG [orExists, negExists, existsexists] [])
  where msg = "Failed to find a model for disjunction fo existentials, "++ 
              "negation of existentials and double existentials"
        target = Left $ sortModel ([1,3,4,2],[("A",[4]),("B",[2])],[("S",[(1,3)]),("R",[(1,2),(3,4)])])
        result = findPOM [orExists, negExists, existsexists] []

testMoreExists3' = testequality msg target result (printCG [forallexists, orforalls, orExists, negExists] [])
  where msg = "Failed to find a model for universal existential, "++
              "disjunction of universal, disjunction of existential and negation of existential"
        target = Left $ sortModel ([1,3,4,2],[("A",[4]),("B",[2])],[("S",[(1,3)]),("R",[(1,2),(3,4)])])
        -- TODO: these are really confusing names, forallexists is an exists exists.
        --       this needs fixing!
        result = findPOM [forallexists, orforalls, orExists, negExists] []

-- should lead to contradiction here i think but doesnt
testMoreExists4' = testequality msg target result (printCG [orExists, negExists, existsexists]  [])
  where msg = "Failed to find a model for disjunction of existentials, "++
              "negation of existentials, and existential of existential"
        target = Left $ sortModel ([1,3,4,2],[("A",[4]),("B",[2])],[("S",[(1,3)]),("R",[(1,2),(3,4)])])
        result = findPOM [orExists, negExists, existsexists] []

--------- Tests for models for non empy knowledge base

testEmptySet' = testequality msg (Left ([1], [], [])) (findPOM [] []) (printCG [] [])
  where msg = "Failed to find a model for empty gamma and no concepts"

testNegAtom' = testequality msg (Left ([1], [], [])) (findPOM [] [notatoma]) (printCG [] [notatoma])
  where msg = "Failed to find a model for gamma negation of an atom"

testAndAtom' = testequality msg (Left ([1],[("A",[1]),("B",[1])],[])) (findPOM [] [a_and_b]) (printCG [] [a_and_b])
  where msg = "Failed to find a model for gamma conjunction"

testOrAtom' = testequality msg (Left ([1],[("A",[1])],[])) (findPOM [] [a_or_b]) (printCG [] [a_or_b])
  where msg = "Failed to find a model for gamma disjunction"

{-testExistsAtom' = testequality msg target (findPOM [] [exists_r_a]) (printCG [] [exists_r_a])
  where msg = "Failed to find a model for gamma existential"
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])-}

testForallAtom' = testequality msg (Left ([1],[],[])) (findPOM [] [forall_r_a]) (printCG [] [forall_r_a])
  where msg = "Failed to find a model for gamma universal"

testMoreExists1'' = testequality msg target (findPOM [] [atoma, exists_r_a]) (printCG [] [atoma, exists_r_a])
  where msg = "Failed to find a model for gamma universal and an atom"
        target = Left $ sortModel ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreExists2'' = testequality msg target (findPOM [] [atoma, exists_r_b, exists_r_a]) (printCG [] [atoma, exists_r_b, exists_r_a])
  where msg = "Failed to find a model for gamma universal, exisential and an atom"
        target = Left $ sortModel ([1,2],[("A",[2]), ("B",[3])],[("R",[(1,2), (1,3)])])

testMoreExists3'' = testequality msg target (findPOM [] [a_or_b, exists_r_b]) (printCG [] [a_or_b, exists_r_b])
  where msg = "Failed to find a model for gamma disjunction and existential 1"
        target = Left $ sortModel ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreExists4'' = testequality msg target (findPOM [] [a_or_b, exists_r_b]) (printCG [] [a_or_b, exists_r_b])
  where msg = "Failed to find a model for gamma disjunction and existential 1"
        target = Left $ sortModel ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreForall1'' = testequality msg target (findPOM [] [atoma, exists_r_a, forall_r_b]) (printCG [] [atoma, exists_r_a, forall_r_b])
  where msg = "Failed to find a model for gamma atomic, existential and universal"
        target =  Left $ sortModel ([1,2],[("A",[2])],[("R",[(1,2)])])







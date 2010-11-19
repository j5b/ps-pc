{- 
   Author: Saguy Benaim
   License: GPL 3.0
   Description: test proof searching
-}

module ProofSearch_test where

import ProofSearch
import Signature
import TestUtils

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
			     testExistsAtom', testForallAtom']

morecomplexModelTests = maplabel "Finding complex models with POM Searcher" 
                                 [testMoreExists1', testMoreExists2', 
                                 testMoreExists3', testMoreExists4']

------------------------------------- Tests for individual functions -------------------------------------------------------

-- conceptSort tests

testConceptSortEmpty = testequality "Failed to sort empty set" [] (conceptSort []) ("[]")

testConceptSortList = testequality msg target result (show input)
  where msg = "Failed to sort  some random list, but with no contradictions" 
        input = [forallAtomc, andAtom, negAtom, existsAtomb, existsAtomc, orExists, c, b, existsAtoma, forallAtoma, orforalls]
        target = [andAtom, orExists, orforalls,  existsAtomb, existsAtomc, existsAtoma, forallAtomc, negAtom, c, b, forallAtoma]
        result = conceptSort input

-- if duplicate in contradictions, do we just take one pair?
testConceptSortContradictions = testequality msg target result $ show input
  where msg = "Failed to sort random list with contradictions"
        input = [forallAtomc, andAtom, negAtomc, existsAtomb, existsAtomc, 
           orExists, c, b, existsAtoma, negAtomc, forallAtoma, negAtomc, negAtomb, negAtomb, orforalls]  
        target = [c, negAtomc, b, negAtomb, andAtom, orExists, orforalls,  existsAtomb, existsAtomc, 
          existsAtoma, forallAtomc, negAtomc, forallAtoma, negAtomc, negAtomb ]
        result = conceptSort input
 
testConceptSortNotSorting = testequality msg target result $ show input
  where msg = "Failed to sort list with negation of atoms"
        input = [forallAtoma, orAtom, Neg orAtom]
        target = [orAtom, forallAtoma, Neg orAtom]
        result = conceptSort input

-- joinModels tests

emptyModel = ([], [], [])
simpleModel1 = ([1,2], [("A",[2])], [("R",[(1,2)])])
simpleModel2 = ([1,3], [("A",[3])], [("R",[(1,3)])])
simpleModel3 = ([1,4], [("A",[4])], [("R",[(1,4)])])
simpleModel4 = ([1,2], [("B",[2])], [("R",[(1,2)])])
model1 = ([1,2,3,4,6], [("B",[1, 3]), ("A",[4, 6]), ("C",[2]), ("C",[3])], [("R",[(1,2), (1, 3)]), ("R1",[(1,4), (2, 6)])])
model2 = ([1,4,5,6,7,9,10], [("B",[1, 7]), ("A",[1, 7, 8]), ("C",[4, 10]), ("C",[4])], [("R",[(10,2), (1, 10)]), ("R1",[(1,5), (5, 6)])])

testJoinModelsEmpty = 
  testequality "Failed to join empty models" emptyModel (joinModels emptyModel emptyModel) input 
    where input = (printModel emptyModel)++"\n\t\tand\n"++(printModel emptyModel) 

testJoinModelsBase1 = 
  testequality "Failed to join simple models 1" simpleModel2 (joinModels simpleModel2 emptyModel) input
    where input = (printModel simpleModel2)++"\n\t\tand\n"++(printModel emptyModel)

testJoinModelsBase2 = 
  testequality "Failed to join simple models 2" model1 (joinModels emptyModel model1) input
    where input = (printModel emptyModel)++"\n\t\tand\n"++(printModel model1)

-- Simple with simple: should we apply sorting to individuals, tuples to get unique nomether order of calling?
-- Also can it ever be that "C" is defined twice as below within a model?
testJoinModelsSimple12 = 
  testequality "Failed to join simple models 3" target (joinModels simpleModel1 simpleModel2) input
    where target = ([1,2,3],[("A",[3,2])],[("R",[(1,3),(1,2)])])
          input = (printModel simpleModel1)++"\n\t\tand\n"++(printModel simpleModel2)

testJoinModelsSimple21 = 
  testequality "Failed to join simple models 4" target (joinModels simpleModel2 simpleModel1) input
    where target = ([1,3,2],[("A",[2,3])],[("R",[(1,2),(1,3)])])
          input = (printModel simpleModel2)++"\n\t\tand\n"++(printModel simpleModel1)

testJoinModelsSimple13 = 
  testequality "Failed to join simple models 5" target (joinModels simpleModel1 simpleModel3) input
    where target = ([1,2,4],[("A",[4,2])],[("R",[(1,4),(1,2)])])
          input = (printModel simpleModel1)++"\n\t\tand\n"++(printModel simpleModel2)

testJoinModelsSimple24 = 
  testequality "Failed to join simple models 6" target (joinModels simpleModel2 simpleModel4) input
    where target = ([1,3,2],[("A",[3]),("B",[2])],[("R",[(1,2),(1,3)])])
          input = (printModel simpleModel2)++"\n\t\tand\n"++(printModel simpleModel4)

testJoinModelsSimple34 = 
  testequality "Failed to join simple models 7" target (joinModels simpleModel3 simpleModel4) input
    where target = ([1,4,2],[("A",[4]),("B",[2])],[("R",[(1,2),(1,4)])])
          input = (printModel simpleModel3)++"\n\t\tand\n"++(printModel simpleModel4)

testJoinModelsComplex1 = 
  testequality "Failed to join complex models 8" target (joinModels simpleModel1 model1) input
    where target = ([1,2,3,4,6],[("A",[4,6,2]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3)]),("R1",[(1,4),(2,6)])])
          input = (printModel simpleModel1)++"\n\t\tand\n"++(printModel model1)

testJoinModelsComplex2 = 
  testequality "Failed to join complex models 9" target (joinModels simpleModel3 model1) input
    where target = ([1,4,2,3,6],[("A",[4,6]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3),(1,4)]),("R1",[(1,4),(2,6)])])
          input = (printModel simpleModel3)++"\n\t\tand\n"++(printModel model1)

testJoinModelsComplex3 = 
  testequality "Failed to join complex models 10" target (joinModels simpleModel2 model2) input
    where target = ([1,3,4,5,6,7,9,10],[("A",[1,7,8,3]),("B",[1,7]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,3)]),("R1",[(1,5),(5,6)])])
          input = (printModel simpleModel2)++"\n\t\tand\n"++(printModel model2)

testJoinModelsComplex4 = 
  testequality "Failed to join complex models 11" target (joinModels simpleModel4 model2) input
    where target = ([1,2,4,5,6,7,9,10],[("B",[1,7,2]),("A",[1,7,8]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,2)]),("R1",[(1,5),(5,6)])])
          input = (printModel simpleModel4)++"\n\t\tand\n"++(printModel model2)

testJoinModelsComplex5 = 
  testequality "Failed to join complex models 12" target (joinModels model1 model2) input
    where target = ([1,2,3,4,6,5,7,9,10],[("B",[1,7,3]),("A",[1,7,8,4,6]),("C",[4,10,2]),("C",[3])],[("R",[(10,2),(1,10),(1,2),(1,3)]),("R1",[(1,5),(5,6),(1,4),(2,6)])])
          input = (printModel model1)++"\n\t\tand\n"++(printModel model2)

-- constructAtomicModel

testConstructAtomicModelEmpty = 
  testequality msg ([2], [], []) (constructAtomicModel [] 2) input
    where msg = "Failed to construct an atomic model containing 2 satisfying no concepts"
          input = ("[]")++" "++(show 2)

testConstructAtomicModelSimple = 
  testequality msg ([10],[("A",[10]),("C",[10])],[]) (constructAtomicModel [a, c, orAtom] 10) input
    where msg = "Failed to construct an atomic model containing 10 satisfying various but simple concepts"
          input = (show [a,c, orAtom])++" "++(show 10)

testConstructAtomicModelBigger = 
  testequality msg ([0],[("A",[0]),("C",[0]),("C",[0]),("B",[0])],[]) result inputString
    where msg = "Failed to construct an atomic model containing 10 satisfying various and complex concepts"
          result = constructAtomicModel [a, c, existsAtoma, c, negAtomc, b, negAtomb, andAtom, orExists] 0
          inputString = (show [a, c, existsAtoma, c, negAtomc, b, negAtomb, andAtom, orExists])++" "++(show 0) 

-- applyExists

prints :: [Concept] -> [Concept] -> Concept -> String
prints c1 c2 c3 = (show c1)++" "++(show c2)++" "++(show c3)

testApplyExistsEmpty = 
  testequality msg [a] (applyExists [] [] (exists "R" a)) $ prints [] [] (exists "R" a)
    where msg = "Failed to apply exists rule with no concepts"

testApplyExistsSimple = 
  testequality msg [a,b,c] (applyExists [forallAtoma, forallAtomb, orAtom, existsAtomc, forallAtomc] [] (exists "R" a)) input
    where msg = "Failed to apply exists rule with a few forall concepts"
          input = prints [forallAtoma, forallAtomb, orAtom, existsAtomc, forallAtomc] [] (exists "R" a)

testApplyExistsOther1 = 
  testequality msg [top, bottom] (applyExists [forall "R" bottom] [] (exists "R" top)) input
    where msg = "Failed to apply exists rule with one forall concept p1"
          input = prints [forall "R" bottom] [] (exists "R" top)

testApplyExistsOther2 = 
  testequality msg [bottom, top] (applyExists [forall "R" top] [] (exists "R" bottom)) input
    where msg = "Failed to apply exists rule with one forall concept p2" 
          input = prints [forall "R" top] [] (exists "R" bottom)

testApplyExistsOther3 = 
  testequality msg [Atom "A",Forall "R" b,Atom "C", exists "R" d] result input
    where msg = "Failed to apply exists rule with non empty gamma and a few concepts"
          result = applyExists [forall "R" c, exists "R" d ] [forall "R" b] (exists "R" a)
          input = prints [forall "R" c, exists "R" d ] [forall "R" b] (exists "R" a)

------------------------------------- Tests of findPOM for models ----------------------------------------------------------------

----------- tests with empty knowlege base

-- base casses

testEmptySet = testequality msg (Left ([1], [], [])) (findPOM [] []) (printCG [] [])
  where msg = "Failed to find model for empty gamma and no concepts"

testNegAtom = testequality msg (Left ([1], [], [])) (findPOM [negAtom] []) (printCG [negAtom] [])
  where msg = "Failed to find a model for the negation of an atom"

testAndAtom = testequality msg (Left ([1],[("A",[1]),("B",[1])],[])) (findPOM [andAtom] []) (printCG [andAtom] [])
  where msg = "Failed to find a model for a conjunction"

testOrAtom  = testequality msg (Left ([1],[("A",[1])],[])) (findPOM [orAtom] []) (printCG [orAtom] [])
  where msg = "Failed to find a model for a disjunction"

testExistsAtom = testequality msg (Left ([1,2],[("A",[2])],[("R",[(1,2)])])) (findPOM [existsAtoma] []) (printCG [existsAtoma] [])
  where msg = "Failed to find a model for existential concept"

testForallAtom = testequality msg (Left ([1],[],[])) (findPOM [forallAtoma] []) (printCG [forallAtoma] [])
  where msg = "Failed to find a model for universal concept"

-- test falsity here

testMoreExists1 = testequality msg target (findPOM [a, existsAtoma] []) (printCG [a, existsAtoma] [])
  where msg = "Failed to find a model for existential concept and an atom" 
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreExists2 = testequality msg target (findPOM [a, existsAtoma, existsAtomb] []) (printCG [a, existsAtoma, existsAtomb] [])
  where msg = "Failed to find a model for two existential concepts and an atom"
        target = Left ([1,3,2],[("B",[3]),("A",[2])],[("R",[(1,2),(1,3)])])

testMoreExists3 = testequality msg target (findPOM [orAtom, existsAtomb] []) (printCG [orAtom, existsAtomb] [])
  where msg = "Failed to find a model for existential concept and a disjunction"
        target = Left ([1,2],[("B",[2])],[("R",[(1,2)])])

testMoreExists4 = testequality msg target (findPOM [orAtom, b, existsAtomb] []) (printCG [orAtom, b, existsAtomb] [])
  where msg = "Failed to find a model for existential, disjunction and an atom 1"
        target = Left ([1,2],[("B",[2])],[("R",[(1,2)])])

testMoreExists5 = testequality msg target (findPOM [a, orAtom, existsAtomb] []) (printCG [a, orAtom, existsAtomb] [])
  where msg = "Failed to find a model for existential, disjunction and an atom 2"
        target= Left ([1,2],[("A",[1]), ("B",[2])],[("R",[(1,2)])])

testMoreForall1 = testequality msg target (findPOM [a, existsAtoma, forallAtomb] []) (printCG [a, existsAtoma, forallAtomb] [])
  where msg = "Failed to find a model for existential, univeral, and atom"
        target =Left ([1,2],[("B",[2]),("A",[2])],[("R",[(1,2)])])

testMore1 = testequality msg target result (printCG concepts [])
  where msg = "Failed to find a model for multiple universals, multiple existentials, and duplicate atomics"
        target = Left ([1,2],[("B",[2]),("A",[2])],[("R",[(1,2)])])
        result = findPOM concepts []
        concepts = [a, existsAtoma, forallAtomb, existsAtoma, a, forallAtomb, a, a, a, forallAtomb, existsAtoma]

testMore2 = testequality msg target result (printCG [a, existsAtoma, existsAtomb, forallAtomc] [])
  where msg = "Failed to find a model for univeral, multiple existentials, and atoms"
        target = Left ([1,3,2],[("C",[2,3]),("B",[3]),("A",[2])],[("R",[(1,2),(1,3)])])
        result = findPOM [a, existsAtoma, existsAtomb, forallAtomc] []

-- some more complex ones

testMoreExists1' = testequality msg target result (printCG [andExists, andforalls] [])
  where msg = "Failed to find a model for conjunction of existentials and conjuctions of universals"
        target = Left ([1,3,2],[("A",[2,3]),("B",[2,3])],[("R",[(1,2),(1,3)])])
        result = findPOM [andExists, andforalls] []

testMoreExists2' = testequality msg target result (printCG [orExists, negExists, existsexists] [])
  where msg = "Failed to find a model for disjunction fo existentials, "++ 
              "negation of existentials and double existentials"
        target = Left ([1,3,4,2],[("A",[4]),("B",[2])],[("R1",[(1,3)]),("R",[(1,2),(3,4)])])
        result = findPOM [orExists, negExists, existsexists] []

testMoreExists3' = testequality msg target result (printCG [forallexists, orforalls, orExists, negExists] [])
  where msg = "Failed to find a model for universal existential, "++
              "disjunction of universal, disjunction of existential and negation of existential"
        target = Left ([1,2],[("B",[2])],[("R",[(1,2)])])
        result = findPOM [forallexists, orforalls, orExists, negExists] []

-- should lead to contradiction here i think but doesnt
testMoreExists4' = testequality msg target result (printCG [orExists, negExists, existsexists]  [])
  where msg = "Failed to find a model for disjunction of existentials, "++
              "negation of existentials, and existential of existential"
        target = Left ([1,2],[("B",[2])],[("R",[(1,2)])])
        result = findPOM [orExists, negExists, existsexists] []

--------- Tests for models for non empy knowledge base

testEmptySet' = testequality msg (Left ([1], [], [])) (findPOM [] []) (printCG [] [])
  where msg = "Failed to find a model for empty gamma and no concepts"

testNegAtom' = testequality msg (Left ([1], [], [])) (findPOM [] [negAtom]) (printCG [] [negAtom])
  where msg = "Failed to find a model for gamma negation of an atom"

testAndAtom' = testequality msg (Left ([1],[("A",[1]),("B",[1])],[])) (findPOM [] [andAtom]) (printCG [] [andAtom])
  where msg = "Failed to find a model for gamma conjunction"

testOrAtom' = testequality msg (Left ([1],[("A",[1])],[])) (findPOM [] [orAtom]) (printCG [] [orAtom])
  where msg = "Failed to find a model for gamma disjunction"

testExistsAtom' = testequality msg target (findPOM [] [existsAtoma]) (printCG [] [existsAtoma])
  where msg = "Failed to find a model for gamma existential"
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])

testForallAtom' = testequality msg (Left ([1],[],[])) (findPOM [] [forallAtoma]) (printCG [] [forallAtoma])
  where msg = "Failed to find a model for gamma universal"

testMoreExists1'' = testequality msg target (findPOM [] [a, existsAtoma]) (printCG [] [a, existsAtoma])
  where msg = "Failed to find a model for gamma universal and an atom"
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreExists2'' = testequality msg target (findPOM [] [a, existsAtomb, existsAtoma]) (printCG [] [a, existsAtomb, existsAtoma])
  where msg = "Failed to find a model for gamma universal, exisential and an atom"
        target = Left ([1,2],[("A",[2]), ("B",[3])],[("R",[(1,2), (1,3)])])

testMoreExists3'' = testequality msg target (findPOM [] [orAtom, existsAtomb]) (printCG [] [orAtom, existsAtomb])
  where msg = "Failed to find a model for gamma disjunction and existential 1"
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreExists4'' = testequality msg target (findPOM [] [orAtom, existsAtomb]) (printCG [] [orAtom, existsAtomb])
  where msg = "Failed to find a model for gamma disjunction and existential 1"
        target = Left ([1,2],[("A",[2])],[("R",[(1,2)])])

testMoreForall1'' = testequality msg target (findPOM [] [a, existsAtoma, forallAtomb]) (printCG [] [a, existsAtoma, forallAtomb])
  where msg = "Failed to find a model for gamma atomic, existential and universal"
        target =  Left ([1,2],[("A",[2])],[("R",[(1,2)])])







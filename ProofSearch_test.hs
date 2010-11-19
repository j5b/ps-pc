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

joinModelTests = maplabel "Joining models" [testJoinModelsEmpty,
                                       testJoinModelsBase1,
                                       testJoinModelsBase2,
                                       testJoinModelsSimple12,
                                       testJoinModelsSimple21,
                                       testJoinModelsSimple13,
                                       testJoinModelsSimple24,
                                       testJoinModelsSimple34,
                                       testJoinModelsComplex1,
                                       testJoinModelsComplex2,
                                       testJoinModelsComplex3,
                                       testJoinModelsComplex4,
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
                             testAndAtom, testOrAtom,
			     testExistsAtom, testForallAtom]

moreModelTests = maplabel "Finding more models with POM Searcher" [testMoreExists1, testMoreExists2,
                             testMoreExists3, testMoreExists4,
			     testMoreExists5, testMoreForall1,
			     testMore1, testMore2]

simpleModelTestsKnowledgeBase = 
    maplabel "Finding models for non empty knowledge base with POM Searcher" 
             [testEmptySet', testNegAtom',
                             testAndAtom', testOrAtom',
			     testExistsAtom', testForallAtom']

morecomplexModelTests = maplabel "Finding complex models with POM Searcher" 
                                 [testMoreExists1', testMoreExists2', 
                                 testMoreExists3', testMoreExists4']

------------------------------------- Tests for individual functions -------------------------------------------------------

-- conceptSort tests

testConceptSortEmpty = testequality "Failed to sort empty set" [] $ conceptSort []

testConceptSortList = testequality msg target result
  where msg = "Failed to sort  some random list, but with no contradictions" 
        target = [andAtom, orExists, orforalls,  existsAtomb, existsAtomc, existsAtoma, forallAtomc, negAtom, c, b, forallAtoma]
        result = conceptSort [forallAtomc, andAtom, negAtom, existsAtomb, existsAtomc, orExists, c, b, existsAtoma, forallAtoma, orforalls]

-- if duplicate in contradictions, do we just take one pair?
testConceptSortContradictions = testequality msg target result
  where msg = "Failed to sort random list with contradictions"
        target = [c, negAtomc, b, negAtomb, andAtom, orExists, orforalls,  existsAtomb, existsAtomc, 
          existsAtoma, forallAtomc, negAtomc, forallAtoma, negAtomc, negAtomb ]
        result = conceptSort [forallAtomc, andAtom, negAtomc, existsAtomb, existsAtomc, 
           orExists, c, b, existsAtoma, negAtomc, forallAtoma, negAtomc, negAtomb, negAtomb, orforalls]  
 
testConceptSortNotSorting = testequality msg target result
  where msg = "Failed to sort list with negation of atoms"
        target = [orAtom, forallAtoma, Neg orAtom]
        result = conceptSort [forallAtoma, orAtom, Neg orAtom]

-- joinModels tests

emptyModel = ([], [], [])
simpleModel1 = ([1,2], [("A",[2])], [("R",[(1,2)])])
simpleModel2 = ([1,3], [("A",[3])], [("R",[(1,3)])])
simpleModel3 = ([1,4], [("A",[4])], [("R",[(1,4)])])
simpleModel4 = ([1,2], [("B",[2])], [("R",[(1,2)])])
model1 = ([1,2,3,4,6], [("B",[1, 3]), ("A",[4, 6]), ("C",[2]), ("C",[3])], [("R",[(1,2), (1, 3)]), ("R1",[(1,4), (2, 6)])])
model2 = ([1,4,5,6,7,9,10], [("B",[1, 7]), ("A",[1, 7, 8]), ("C",[4, 10]), ("C",[4])], [("R",[(10,2), (1, 10)]), ("R1",[(1,5), (5, 6)])])

testJoinModelsEmpty = 
  testequality "Failed to join empty models" emptyModel $ joinModels emptyModel emptyModel

testJoinModelsBase1 = 
  testequality "Failed to join simple models" simpleModel2 $ joinModels simpleModel2 emptyModel

testJoinModelsBase2 = 
  testequality "Failed to join simple models " model1 $ joinModels emptyModel model1

-- Simple with simple: should we apply sorting to individuals, tuples to get unique nomether order of calling?
-- Also can it ever be that "C" is defined twice as below within a model?
testJoinModelsSimple12 = 
  testequality "Failed to join simple models" target $ joinModels simpleModel1 simpleModel2
    where target = ([1,2,3],[("A",[3,2])],[("R",[(1,3),(1,2)])])

testJoinModelsSimple21 = 
  testequality "Failed to join simple models" target $ joinModels simpleModel2 simpleModel1
    where target = ([1,3,2],[("A",[2,3])],[("R",[(1,2),(1,3)])])

testJoinModelsSimple13 = 
  testequality "Failed to join simple models" target $ joinModels simpleModel1 simpleModel3
    where target = ([1,2,4],[("A",[4,2])],[("R",[(1,4),(1,2)])])

testJoinModelsSimple24 = 
  testequality "Failed to join simple models" target $ joinModels simpleModel2 simpleModel4
    where target = ([1,3,2],[("A",[3]),("B",[2])],[("R",[(1,2),(1,3)])])

testJoinModelsSimple34 = 
  testequality "Failed to join simple models" target $ joinModels simpleModel2 simpleModel4
    where target = ([1,4,2],[("A",[4]),("B",[2])],[("R",[(1,2),(1,4)])])

testJoinModelsComplex1 = 
  testequality "Failed to join complex models" target $ joinModels simpleModel1 model1
    where target = ([1,2,3,4,6],[("A",[4,6,2]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3)]),("R1",[(1,4),(2,6)])])

testJoinModelsComplex2 = 
  testequality "Failed to join complex models" target $ joinModels simpleModel3 model1
    where target = ([1,4,2,3,6],[("A",[4,6]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3),(1,4)]),("R1",[(1,4),(2,6)])])

testJoinModelsComplex3 = 
  testequality "Failed to join complex models" target $ joinModels simpleModel2 model2
    where target = ([1,3,4,5,6,7,9,10],[("A",[1,7,8,3]),("B",[1,7]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,3)]),("R1",[(1,5),(5,6)])])

testJoinModelsComplex4 = 
  testequality "Failed to join complex models" target $ joinModels simpleModel4 model2
    where target = ([1,2,4,5,6,7,9,10],[("B",[1,7,2]),("A",[1,7,8]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,2)]),("R1",[(1,5),(5,6)])])

testJoinModelsComplex5 = 
  testequality "Failed to join complex models" target $ joinModels model1 model2
    where target = ([1,2,3,4,6,5,7,9,10],[("B",[1,7,3]),("A",[1,7,8,4,6]),("C",[4,10,2]),("C",[3])],[("R",[(10,2),(1,10),(1,2),(1,3)]),("R1",[(1,5),(5,6),(1,4),(2,6)])])

-- constructAtomicModel

testConstructAtomicModelEmpty = 
  testequality msg ([2], [], []) $ constructAtomicModel [] 2
    where msg = "Failed to construct an atomic model containing 2 satisfying no concepts"

testConstructAtomicModelSimple = 
  testequality msg ([10],[("A",[10]),("C",[10])],[]) $ constructAtomicModel [a, c, orAtom] 10
    where msg = "Failed to construct an atomic model containing 10 satisfying various but simple concepts"

testConstructAtomicModelBigger = 
  testequality msg ([0],[("A",[0]),("C",[0]),("C",[0]),("B",[0])],[]) result
    where msg = "Failed to construct an atomic model containing 10 satisfying various and complex concepts"
          result = constructAtomicModel [a, c, existsAtoma, c, negAtomc, b, negAtomb, andAtom, orExists] 0

-- applyExists

testApplyExistsEmpty = 
  testequality msg [a] $ applyExists [] [] (exists "R" a)
    where msg = "Failed to apply exists rule with no concepts"

testApplyExistsSimple = 
  testequality msg [a,b,c] $ applyExists [forallAtoma, forallAtomb, orAtom, existsAtomc, forallAtomc] [] (exists "R" a)
    where msg = "Failed to apply exists rule with a few forall concepts"

testApplyExistsOther1 = 
  testequality msg [top, bottom] $ applyExists [forall "R" bottom] [] (exists "R" top)
    where msg = "Failed to apply exists rule with one forall concept p1"

testApplyExistsOther2 = 
  testequality msg [bottom, top] $ applyExists [forall "R" top] [] (exists "R" bottom)
    where msg = "Failed to apply exists rule with one forall concept p2" 

testApplyExistsOther3 = 
  testequality msg [Atom "A",Forall "R" b,Atom "C", exists "R" d] result
    where msg = "Failed to apply exists rule with non empty gamma and a few concepts"
          result = applyExists [forall "R" c, exists "R" d ] [forall "R" b] (exists "R" a)

------------------------------------- Tests of findPOM for models ----------------------------------------------------------------

----------- tests with empty knowlege base

-- base casses

testEmptySet = 
   TestCase (assertEqual "Corret empty set of concepts"
   (Left ([1], [], []))                
   (findPOM [] []))

testNegAtom =
   TestCase (assertEqual "Corret simple atom negation model"
   (Left ([1], [], []))                
   (findPOM [negAtom] []))

testAndAtom =
   TestCase (assertEqual "Corret simple atom conjunction model"
   (Left ([1],[("A",[1]),("B",[1])],[]))                
   (findPOM [andAtom] []))

testOrAtom =
   TestCase (assertEqual "Corret simple atom disjunction model"
   (Left ([1],[("A",[1])],[]))                
   (findPOM [orAtom] []))

testExistsAtom =
   TestCase (assertEqual "Corret simple atom exists model"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [existsAtoma] []))

testForallAtom =
   TestCase (assertEqual "Corret simple atom forall model"
   (Left ([1],[],[]))                
   (findPOM [forallAtoma] []))

-- test falsity here

testMoreExists1 =
   TestCase (assertEqual "more exists 1"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [a, existsAtoma] []))

testMoreExists2 =
   TestCase (assertEqual "more exists 2"
   (Left ([1,3,2],[("B",[3]),("A",[2])],[("R",[(1,2),(1,3)])]))                
   (findPOM [a, existsAtoma, existsAtomb] []))

testMoreExists3 =
   TestCase (assertEqual "more exists 3"
   (Left ([1,2],[("B",[2])],[("R",[(1,2)])]))                
   (findPOM [orAtom, existsAtomb] []))

testMoreExists4 =
   TestCase (assertEqual "more exists 4"
   (Left ([1,2],[("B",[2])],[("R",[(1,2)])]))                
   (findPOM [orAtom, b, existsAtomb] []))

-- problem here: a should get assigned to some individual, it isn't!
testMoreExists5 =
   TestCase (assertEqual "more exists 5"
   (Left ([1,2],[("A",[1]), ("B",[2])],[("R",[(1,2)])]))                
   (findPOM [a, orAtom, existsAtomb] []))

testMoreForall1 =
   TestCase (assertEqual "more forall 1"
   (Left ([1,2],[("B",[2]),("A",[2])],[("R",[(1,2)])]))                
   (findPOM [a, existsAtoma, forallAtomb] []))

-- problem here: it seems to generate a new arrow for each existsa, even though existsa is the same
testMore1 =
   TestCase (assertEqual "more 1"
   (Left ([1,2],[("B",[2]),("A",[2])],[("R",[(1,2)])]))                
   (findPOM [a, existsAtoma, forallAtomb, existsAtoma, a, forallAtomb, a, a, a, forallAtomb, existsAtoma] []))

testMore2 =
   TestCase (assertEqual "more 2"
   (Left ([1,3,2],[("C",[2,3]),("B",[3]),("A",[2])],[("R",[(1,2),(1,3)])]))                
   (findPOM [a, existsAtoma, existsAtomb, forallAtomc] []))

-- some more complex ones

testMoreExists1' =
   TestCase (assertEqual "more exists 1"
   (Left ([1,3,2],[("A",[2,3]),("B",[2,3])],[("R",[(1,2),(1,3)])]))                
   (findPOM [andExists, andforalls] []))

testMoreExists2' =
   TestCase (assertEqual "more exists 2"
   (Left ([1,3,4,2],[("A",[4]),("B",[2])],[("R1",[(1,3)]),("R",[(1,2),(3,4)])]))                
   (findPOM [orExists, negExists, existsexists] []))

testMoreExists3' =
   TestCase (assertEqual "more exists 3"
   (Left ([1,2],[("B",[2])],[("R",[(1,2)])]))                
   (findPOM [forallexists, orforalls, orExists, negExists] []))

-- should lead to contradiction here i think but doesnt
testMoreExists4' =
   TestCase (assertEqual "more exists 4"
   (Left ([1,2],[("B",[2])],[("R",[(1,2)])]))                
   (findPOM [orExists, negExists, existsexists]  []))

--------- Tests for models for non empy knowledge base

testEmptySet' = 
   TestCase (assertEqual "Corret empty set of concepts"
   (Left ([1], [], []))                
   (findPOM [] []))

testNegAtom' =
   TestCase (assertEqual "Corret simple atom negation model"
   (Left ([1], [], []))                
   (findPOM [] [negAtom]))

testAndAtom' =
   TestCase (assertEqual "Corret simple atom conjunction model"
   (Left ([1],[("A",[1]),("B",[1])],[]))                
   (findPOM [] [andAtom]))

testOrAtom' =
   TestCase (assertEqual "Corret simple atom disjunction model"
   (Left ([1],[("A",[1])],[]))                
   (findPOM [] [orAtom]))

testExistsAtom' =
   TestCase (assertEqual "Corret simple atom exists model"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [] [existsAtoma]))

testForallAtom' =
   TestCase (assertEqual "Corret simple atom forall model"
   (Left ([1],[],[]))                
   (findPOM [] [forallAtoma]))

testMoreExists1'' =
   TestCase (assertEqual "more exists 1"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [] [a, existsAtoma]))

testMoreExists2'' =
   TestCase (assertEqual "more exists 2"
   (Left ([1,2],[("A",[2]), ("B",[3])],[("R",[(1,2), (1,3)])]))                
   (findPOM [] [a, existsAtomb, existsAtoma]))

testMoreExists3'' =
   TestCase (assertEqual "more exists 3"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [] [orAtom, existsAtomb]))

testMoreExists4'' =
   TestCase (assertEqual "more exists 4"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [] [orAtom, existsAtomb]))

testMoreForall1'' =
   TestCase (assertEqual "more forall 1"
   (Left ([1,2],[("A",[2])],[("R",[(1,2)])]))                
   (findPOM [] [a, existsAtoma, forallAtomb]))







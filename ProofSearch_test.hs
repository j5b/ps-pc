{- 
   Author: Saguy Benaim
   License: GPL 3.0
   Description: test proof searching
-}

import ProofSearch
import Signature
import Test.HUnit
import TestUtils

------------------------------------- Tests for individual functions -------------------------------------------------------

-- conceptSort tests

testConceptSortEmpty = 
   TestCase (assertEqual "Empty set"
   ([])                
   (conceptSort []))

testConceptSortList = 
   TestCase (assertEqual "Random list, but no contradictions"
   ([andAtom, orExists, orforalls,  existsAtomb, existsAtomc, existsAtoma, forallAtomc, negAtom, c, b, forallAtoma ])                
   (conceptSort [forallAtomc, andAtom, negAtom, existsAtomb, existsAtomc, orExists, c, b, existsAtoma, forallAtoma, orforalls]))

-- if duplicate in contradictions, do we just take one pair?
testConceptSortContradictions = 
   TestCase (assertEqual "Random list with contradictions"
   ([c, negAtomc, b, negAtomb, andAtom, orExists, orforalls,  existsAtomb, existsAtomc, 
          existsAtoma, forallAtomc, negAtomc, forallAtoma, negAtomc, negAtomb ])                
   (conceptSort [forallAtomc, andAtom, negAtomc, existsAtomb, existsAtomc, 
           orExists, c, b, existsAtoma, negAtomc, forallAtoma, negAtomc, negAtomb, negAtomb, orforalls]))

testConceptSortNotSorting = 
   TestCase (assertEqual "Not sorting not Atoms"
   ([orAtom, forallAtoma, Neg orAtom])                
   (conceptSort [forallAtoma, orAtom, Neg orAtom]))

conceptSortTests = TestList [TestLabel "conceptSort1" testConceptSortEmpty, 
                            TestLabel "conceptSort2" testConceptSortList, 
                            TestLabel "conceptSort3" testConceptSortContradictions, 
                            TestLabel "conceptSort4" testConceptSortNotSorting]

-- joinModels tests

emptyModel = ([], [], [])
simpleModel1 = ([1,2], [("A",[2])], [("R",[(1,2)])])
simpleModel2 = ([1,3], [("A",[3])], [("R",[(1,3)])])
simpleModel3 = ([1,4], [("A",[4])], [("R",[(1,4)])])
simpleModel4 = ([1,2], [("B",[2])], [("R",[(1,2)])])
model1 = ([1,2,3,4,6], [("B",[1, 3]), ("A",[4, 6]), ("C",[2]), ("C",[3])], [("R",[(1,2), (1, 3)]), ("R1",[(1,4), (2, 6)])])
model2 = ([1,4,5,6,7,9,10], [("B",[1, 7]), ("A",[1, 7, 8]), ("C",[4, 10]), ("C",[4])], [("R",[(10,2), (1, 10)]), ("R1",[(1,5), (5, 6)])])

testJoinModelsEmpty = 
   TestCase (assertEqual "Empty model"
   (emptyModel)                
   (joinModels emptyModel emptyModel))

testJoinModelsBase1 = 
   TestCase (assertEqual "Base case 1"
   (simpleModel2)                
   (joinModels simpleModel2 emptyModel))

testJoinModelsBase2 = 
   TestCase (assertEqual "Base case 2"
   (model1)                
   (joinModels emptyModel model1))

-- Simple with simple: should we apply sorting to individuals, tuples to get unique nomether order of calling?
-- Also can it ever be that "C" is defined twice as below within a model?
testJoinModelsSimple12 = 
   TestCase (assertEqual "Simple models"
   (([1,2,3],[("A",[3,2])],[("R",[(1,3),(1,2)])]))                
   (joinModels simpleModel1 simpleModel2))

testJoinModelsSimple21 = 
   TestCase (assertEqual "Simple models reverse"
   (([1,3,2],[("A",[2,3])],[("R",[(1,2),(1,3)])]))                
   (joinModels simpleModel2 simpleModel1))

testJoinModelsSimple13 = 
   TestCase (assertEqual "Simple models"
   (([1,2,4],[("A",[4,2])],[("R",[(1,4),(1,2)])]))                
   (joinModels simpleModel1 simpleModel3))

testJoinModelsSimple24 = 
   TestCase (assertEqual "Simple models"
   (([1,3,2],[("A",[3]),("B",[2])],[("R",[(1,2),(1,3)])]))                
   (joinModels simpleModel2 simpleModel4))

testJoinModelsSimple34 = 
   TestCase (assertEqual "Simple models"
   (([1,4,2],[("A",[4]),("B",[2])],[("R",[(1,2),(1,4)])]))                
   (joinModels simpleModel2 simpleModel4))

testJoinModelsComplex1  = 
   TestCase (assertEqual "Complex models"
   (([1,2,3,4,6],[("A",[4,6,2]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3)]),("R1",[(1,4),(2,6)])]))                
   (joinModels simpleModel1 model1))

testJoinModelsComplex2  = 
   TestCase (assertEqual "Complex models"
   (([1,4,2,3,6],[("A",[4,6]),("B",[1,3]),("C",[2]),("C",[3])],[("R",[(1,2),(1,3),(1,4)]),("R1",[(1,4),(2,6)])]))                
   (joinModels simpleModel3 model1))

testJoinModelsComplex3  = 
   TestCase (assertEqual "Complex models"
   (([1,3,4,5,6,7,9,10],[("A",[1,7,8,3]),("B",[1,7]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,3)]),("R1",[(1,5),(5,6)])]))                
   (joinModels simpleModel2 model2))

testJoinModelsComplex4 =
   TestCase (assertEqual "Complex models"
   (([1,2,4,5,6,7,9,10],[("B",[1,7,2]),("A",[1,7,8]),("C",[4,10]),("C",[4])],[("R",[(10,2),(1,10),(1,2)]),("R1",[(1,5),(5,6)])]))                
   (joinModels simpleModel4 model2))

testJoinModelsComplex5  = 
   TestCase (assertEqual "most complex models"
   (([1,2,3,4,6,5,7,9,10],[("B",[1,7,3]),("A",[1,7,8,4,6]),("C",[4,10,2]),("C",[3])],[("R",[(10,2),(1,10),(1,2),(1,3)]),("R1",[(1,5),(5,6),(1,4),(2,6)])]))                
   (joinModels model1 model2))

-- constructAtomicModel

testConstructAtomicModelEmpty = 
   TestCase (assertEqual "Empty set"
   (([2], [], []))                
   (constructAtomicModel [] 2))

testConstructAtomicModelSimple = 
   TestCase (assertEqual "Simple set"
   (([10],[("A",[10]),("C",[10])],[]))                
   (constructAtomicModel [a, c, orAtom] 10))

testConstructAtomicModelBigger = 
   TestCase (assertEqual "Bigger set"
   (([0],[("A",[0]),("C",[0]),("C",[0]),("B",[0])],[]))   
   (constructAtomicModel [a, c, existsAtoma, c, negAtomc, b, negAtomb, andAtom, orExists] 0))

constructAtomicModelTests = TestList [TestLabel "constructAtomicModel1" testConstructAtomicModelEmpty, 
                            TestLabel "constructAtomicModel2" testConstructAtomicModelSimple, 
                            TestLabel "constructAtomicModel3" testConstructAtomicModelBigger]

-- applyExists

testApplyExistsEmpty = 
   TestCase (assertEqual "Empty set"
   ([a])                
   (applyExists [] [] (exists "R" a)))

testApplyExistsSimple = 
   TestCase (assertEqual "A lot of forall atoms"
   ([a,b,c])                
   (applyExists [forallAtoma, forallAtomb, orAtom, existsAtomc, forallAtomc] [] (exists "R" a)))

testApplyExistsOther1 = 
   TestCase (assertEqual "More stuff"
   ([top, bottom])                
   (applyExists [forall "R" bottom] [] (exists "R" top)))

testApplyExistsOther2 = 
   TestCase (assertEqual "More stuff"
   ([bottom, top])                
   (applyExists [forall "R" top] [] (exists "R" bottom)))

testApplyExistsOther3 = 
   TestCase (assertEqual "More stuff"
   ([Atom "A",Forall "R" b,Atom "C", exists "R" d])                
   (applyExists [forall "R" c, exists "R" d ] [forall "R" b] (exists "R" a)))



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

simpleModelTests = TestList [TestLabel "simpemodel1" testEmptySet,
                            TestLabel "simpemodel2" testNegAtom,
                            TestLabel "simpemodel3" testAndAtom,
       			    TestLabel "simpemodel4" testOrAtom,
			    TestLabel "simpemodel5" testExistsAtom,
			    TestLabel "simpemodel6" testForallAtom]

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

moreModelTests = TestList [TestLabel "model1" testMoreExists1,
                            TestLabel "model2" testMoreExists2,
                            TestLabel "model3" testMoreExists3,
       			    TestLabel "model4" testMoreExists4,
			    TestLabel "model5" testMoreExists5,
			    TestLabel "model6" testMoreForall1,
			    TestLabel "model7" testMore1,
			    TestLabel "model8" testMore2]

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

morecomplexModelTests = TestList [TestLabel "model1" testMoreExists1',
                            TestLabel "model2" testMoreExists2',
                            TestLabel "model3" testMoreExists3',
       			    TestLabel "model4" testMoreExists4']

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

simpleModelTestsKnowledgeBase = TestList [TestLabel "simpemodel1" testEmptySet',
                            TestLabel "simpemodel2" testNegAtom',
                            TestLabel "simpemodel3" testAndAtom',
       			    TestLabel "simpemodel4" testOrAtom',
			    TestLabel "simpemodel5" testExistsAtom',
			    TestLabel "simpemodel6" testForallAtom']

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







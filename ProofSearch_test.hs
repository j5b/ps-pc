{- 
   Author: Saguy Benaim
   License: GPL 3.0
   Description: test proof searching
-}

import ProofSearch
import Signature
import Test.HUnit

-- Some simple concepts here
--a, b, c, d, e, andAtom, negAtom, orAtom, exists :: Concept
a = Atom "A"
b = Atom "B"
c = Atom "C"
d = Atom "D"
e = Atom "E"
negAtom = Neg a
andAtom = And a b
orAtom = Or a b
existsAtoma = Exists "R" a
existsAtomb = Exists "R" b
forallAtoma = Forall "R" a
forallAtomb = Forall "R" b
forallAtomc = Forall "R" c
andExists = And existsAtoma existsAtomb
andforalls = And forallAtoma forallAtomb
orExists = Or existsAtoma existsAtomb
orforalls = Or forallAtoma forallAtomb
negExists = Neg existsAtoma
negForalls = Neg forallAtoma
existsexists = Exists "R1" existsAtoma
existsforalls = Exists "R1" forallAtoma
forallexists = Forall "R1" existsAtoma

-- Tests for individual functions



-- Tests for models
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

{-andExists = And existsAtoma existsAtomb
andforalls = And forallAtoma forallAtomb
orExists = Or existsAtoma existsAtomb
orforalls = Or forallAtoma forallAtomb
negExists = Neg existsAtoma
negForalls = Neg forallAtoma
existsexists = Exists "R1" existsAtoma
existsforalls = Exists "R1" forallAtoma
forallexists = Forall "R1" existsAtoma-}

testMoreExists1' =
   TestCase (assertEqual "more exists 1"
   (Left ([1,3,2],[("A",[2,3]),("B",[2,3])],[("R",[(1,2),(1,3)])]))                
   (findPOM [andExists, andforalls] []))

-- wow it actually works!
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

----------------------------------------------------------------------

-- Tests for models for non empy knowledge base
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







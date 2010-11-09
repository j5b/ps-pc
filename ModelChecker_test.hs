{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: tests the model checker
-}

import Test.HUnit
import Data.Maybe
import ModelChecker
import Model
import Signature

testLabel (a,b) = TestLabel a b

---- ATOMIC TESTING ----

-- empty model and satisfying truth
atomicTest1 = TestCase ( assertBool "truth satisfied in empty model" $ target == result)
  where target = False
        result = checkAtomic top emptyModel 0

simpleConcept1 = atom "C"
simpleConcept2 = atom "D"
simpleModel = ( [1,2], [("C",[1,2]),("D",[2])], [("R", [(1,2),(2,1)])] )

atomicTest2 = TestCase ( assertBool "some simple concept working in simple model" $ target == result)
  where target = True
        result = checkAtomic simpleConcept1 simpleModel 1

atomicTest3 = TestCase ( assertBool "Some slight nastier concept not working in a simple model" $ target == result)
 where target = False
       result = checkAtomic (neg simpleConcept2) simpleModel 2

atomicTest4 = TestCase ( assertBool "Some slight nastier concept not working in a simple model" $ target == result)
 where target = True
       result = checkAtomic (neg simpleConcept2) simpleModel 1

atomicTests = TestList $ map testLabel $ [("Simplest test of all", atomicTest1),
                                          ("Slighty more complicated test", atomicTest2),
                                          ("Slight nastier test", atomicTest3),
                                          ("Even nastier test", atomicTest4)]

---- SIMPLE FORMULAS TESTING ----

-- we will use simple model
simpleTest1 = TestCase ( assertEqual "In simple model validates C and D" target result )
  where target  = True
        result  = checkModel concept simpleModel
        concept = simpleConcept1 /\ simpleConcept2
simpleTest2 = TestCase ( assertBool "In simple model not validates not C and D" $ target == result )
  where target  = False
        result  = checkModel concept simpleModel
        concept = neg simpleConcept1 /\ simpleConcept2
simpleTest3 = TestCase ( assertBool "In simple model Truth holds" $ target == result )
  where target  = True
        result  = checkModel top simpleModel
simpleTest4 = TestCase ( assertBool "In simple model Bottom not holds" $ target == result )
  where target  = False
        result  = checkModel bottom simpleModel
simpleTest5 = TestCase ( assertBool "In simple model validates C or D" $ target == result)
  where target  = True
        result  = checkModel concept simpleModel
        concept = simpleConcept1 \/ simpleConcept2
simpleTest6 = TestCase ( assertBool "In simple model validates not C or not D" $ target == result)
  where target  = True
        result  = checkModel concept simpleModel
        concept = (neg simpleConcept1) \/ (neg simpleConcept2)

simpleTests = TestList $ map testLabel $ [("And test 1", simpleTest1),
                                          ("And test 2", simpleTest2),
                                          ("Truth test", simpleTest3),
                                          ("Bottom test", simpleTest4),
                                          ("Or test 1", simpleTest5),
                                          ("Or test 2", simpleTest6)]

niceConcept1 = forall "R" (atom "C")
niceConcept2 = exists "R" (atom "D")
niceConcept3 = forall "R" top
niceConcept4 = exists "R" bottom -- false
niceConcept5 = niceConcept3 /\ niceConcept2
niceConcept6 = exists "R" ((atom "C") /\ (atom "D"))

niceTest1 = TestCase (assertEqual "In simple model forall R C" True result)
  where result = checkModel niceConcept1 simpleModel
niceTest2 = TestCase (assertEqual "In simple model exists R D" True result)
  where result = checkModel niceConcept2 simpleModel
niceTest3 = TestCase (assertEqual "In simple model forall R top" True result)
  where result = checkModel niceConcept3 simpleModel
niceTest4 = TestCase (assertEqual "In simple model exists R bottom" False result)
  where result = checkModel niceConcept4 simpleModel
niceTest5 = TestCase (assertEqual "In simple model forall R top and exists R D" True result)
  where result = checkModel niceConcept5 simpleModel
niceTest6 = TestCase (assertEqual "In simple model exists R C and D" True result)
  where result = checkModel niceConcept6 simpleModel

niceTests = TestList $ map testLabel $ [("nice test 1", niceTest1),
                                        ("nice test 2", niceTest2),
                                        ("nice test 3", niceTest3),
                                        ("nice test 4", niceTest4),
                                        ("nice test 5", niceTest5),
                                        ("nice test 6", niceTest6)]
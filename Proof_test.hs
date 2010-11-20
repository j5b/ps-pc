{- 
   Author: Ka Wai Cheng
   Maintainer: TBD
   Email: TBD
   License: GPL 3.0
   File: Proof_test.hs
   Description:  Tests the Proof.hs
-}

module Proof_test where 

import Signature
import Proof
import ProofUtils
import Test.HUnit

allproofdttests = do putStrLn "==== Testing Proof Data type basic functions"
                     runTestTT conceptEqualsTests
                     runTestTT getConceptTests

-- Some concepts/proof tres to use in tests

bConcept = Neg (Atom "A")
aConcept = And (Atom "A") (Atom "B")
oConcept = Or (Atom "A") (Atom "B")
eConcept = Exists "R" (Atom "A")
fConcept = Forall "R" (Atom "B")

leaf = NodeZero ([Neg T], "", Neg T)
bTree = NodeOne ([Atom "A", Neg (Atom "A")], bRule, Atom "A") leaf
aTree = NodeOne ([aConcept, Neg (Atom "A")], aRule, aConcept)
		(NodeOne ([Atom "A", Atom "B", Neg (Atom "A")], bRule, Atom "A") leaf)
oTree = NodeTwo ([oConcept, Neg (Atom "A"), Neg (Atom "B")], oRule, oConcept)
		(NodeOne ([Atom "A", Neg (Atom "A"), Neg (Atom "B")], bRule, Atom "A") leaf)
		(NodeOne ([Atom "B", Neg (Atom "A"), Neg (Atom "B")], bRule, Atom "B") leaf)
eTree = NodeOne ([Forall "R" (Neg (Atom "A")), eConcept, Forall "R" (Atom "A"),
                 Forall "S" (Atom "B")], eRule, eConcept)
                (NodeOne ([Atom "A", Atom "A", Neg (Atom "A")], bRule, Atom "A") leaf)

-- Tests for conceptEquals
cEqualsTest1 = TestCase(assertBool "conceptEquals 2 empty lists"
                        (conceptEquals [] []))
cEqualsTest2 = TestCase(assertBool "conceptEquals 1 empty list1"
                        (not (conceptEquals [Neg T] [])))
cEqualsTest3 = TestCase(assertBool "conceptEquals 1 empty list2"
                        (not(conceptEquals [] [Neg T])))
cEqualsTest4 = TestCase(assertBool "conceptEquals unequal lists1"
                        (not ((conceptEquals [Neg T, Atom "A"] [Neg T]))))
cEqualsTest5 = TestCase(assertBool "conceptEquals lists with same order"
			            (conceptEquals c1 c2))
  where c1 = [Neg T, Atom "A", aConcept, oConcept, eConcept, Forall "R" (Atom "A")]
        c2 = [Neg T, Atom "A", aConcept, oConcept, eConcept, Forall "R" (Atom "A")]
cEqualsTest6 = TestCase(assertBool "conceptEquals different order"
                              (conceptEquals c1 c2 ))
  where c1 = [Neg T, Atom "A", aConcept, oConcept, eConcept, Forall "R" (Atom "A")]
        c2 = [oConcept, Forall "R" (Atom "A"), eConcept, Neg T, Atom "A", aConcept]

conceptEqualsTests = TestList [TestLabel "conceptEquals [] []" cEqualsTest1,
                               TestLabel "conceptEquals [Neg T] []" cEqualsTest2,
                               TestLabel "conceptEquals [] [Neg T]" cEqualsTest3,
                               TestLabel "conceptEquals unequal lists1" cEqualsTest4,
                               TestLabel "conceptEquals unequal lists2" cEqualsTest5,
                               TestLabel "conceptEquals same order" cEqualsTest6]

-- Tests for getConcepts
gConceptsTest1 = TestCase(assertEqual "getConcept from a Leaf node" [Neg T]
                          (getConcepts leaf))
gConceptsTest2 = TestCase(assertEqual "getConcept from a bTree"
			              [Atom "A", bConcept] (getConcepts bTree))
gConceptsTest3 = TestCase(assertEqual "getConcept from a aTree"
			              [aConcept, Neg (Atom "A")] (getConcepts aTree))
gConceptsTest4 = TestCase(assertEqual "getConcept from a oTree"
			              [oConcept, Neg (Atom "A"), Neg (Atom "B")]
                          (getConcepts oTree))
gConceptsTest5 = TestCase(assertEqual "getConcept from a eTree"
			              [Forall "R" (Neg (Atom "A")), eConcept,
                           Forall "R" (Atom "A"), Forall "S" (Atom "B")]
                          (getConcepts eTree))

getConceptTests = TestList [TestLabel "getConcepts Leaf" gConceptsTest1,
                            TestLabel "getConcepts btree" gConceptsTest2,
                            TestLabel "getConcepts atree" gConceptsTest3,
                            TestLabel "getConcepts otree" gConceptsTest4,
                            TestLabel "getConcepts etree" gConceptsTest5]


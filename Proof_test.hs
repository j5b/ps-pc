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
import TestUtils
import ProofUtils

import Test.HUnit

allproofdttests = do putStrLn "==== Testing Proof Data type basic functions"
                     runTestTT conceptEqualsTests
                     runTestTT getConceptTests

-- Some concepts/proof tres to use in tests

leaf = NodeZero ([bottom], "", bottom)
bTree = NodeOne ([atoma, notatoma], bRule, atoma) leaf
aTree = NodeOne ([a_and_b, notatoma], aRule, a_and_b)
		(NodeOne ([atoma, atomb, neg atoma], bRule, atoma) leaf)
oTree = NodeTwo ([a_or_b, notatoma, notatomb], oRule, a_or_b)
		(NodeOne ([atoma, notatoma, notatomb], bRule, atoma) leaf)
		(NodeOne ([atomb, notatoma, notatomb], bRule, atomb) leaf)
eTree = NodeOne ([forall_r notatoma, exists_r_a, forall_r_a,
                  forall_s atomb], eRule, exists_r_a)
                (NodeOne ([atoma, atoma, notatoma], bRule, atoma) leaf)

-- Tests for conceptEquals
cEqualsTest1 = TestCase(assertBool "conceptEquals 2 empty lists"
                        (conceptEquals [] []))
cEqualsTest2 = TestCase(assertBool "conceptEquals 1 empty list1"
                        (not (conceptEquals [bottom] [])))
cEqualsTest3 = TestCase(assertBool "conceptEquals 1 empty list2"
                        (not(conceptEquals [] [bottom])))
cEqualsTest4 = TestCase(assertBool "conceptEquals unequal lists1"
                        (not (conceptEquals [bottom, atoma] [bottom])))
cEqualsTest5 = TestCase(assertBool "conceptEquals lists with same order"
			            (conceptEquals c1 c2))
  where c1 = [bottom, atoma, a_and_b, a_or_b, exists_r_a, forall_r_a]
        c2 = [bottom, atoma, a_and_b, a_or_b, exists_r_a, forall_r_a]
cEqualsTest6 = TestCase(assertBool "conceptEquals different order"
                              (conceptEquals c1 c2 ))
  where c1 = [bottom, atoma, a_and_b, a_or_b, exists_r_a, forall_r_a]
        c2 = [a_or_b, forall_r_a, exists_r_a, bottom, atoma, a_and_b]

conceptEqualsTests = maplabel "Testing conceptEquals" list 
  where list = [cEqualsTest1, cEqualsTest2, cEqualsTest3, cEqualsTest4, cEqualsTest5, cEqualsTest6]

-- Tests for getConcepts
gConceptsTest1 = TestCase(assertEqual "Failed to getConcept from a Leaf node" [bottom]
                          (getConcepts leaf))
gConceptsTest2 = TestCase(assertEqual "Failed to getConcept from a bottom node"
			              [atoma, notatoma] (getConcepts bTree))
gConceptsTest3 = TestCase(assertEqual "Failed to getConcept from an and node "
			              [a_and_b, notatoma] (getConcepts aTree))
gConceptsTest4 = TestCase(assertEqual "Failed to getConcept from an or node"
			              [a_or_b, notatoma, notatomb]
                          (getConcepts oTree))
gConceptsTest5 = TestCase(assertEqual "Failed to getConcept from an exist node"
			              [forall_r notatoma, exists_r_a,
                                      forall_r atoma, forall_s atomb]
                          (getConcepts eTree))

getConceptTests = maplabel "Testing getConcepts" list 
  where list = [gConceptsTest1, gConceptsTest2, gConceptsTest3, gConceptsTest4, gConceptsTest5]


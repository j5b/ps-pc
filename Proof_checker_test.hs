{- 
   Author: Ka Wai Cheng
   Maintainer: Ka Wai Cheng
   Email: mail.kawai.cheng@gmail.com
   License: GPL 3.0
   File: proof_checker_test.hs
   Description: tests for the Proof Checker
-}

import Signature
import Proof
import Proof_checker
import Test.HUnit

bRule = "Bottom"
aRule = "And"
oRule = "Or"
eRule = "Exists"

-- Test proofstep structures:
bStep1 = ([Atom "A", Neg (Atom "A")], bRule, Atom "A")
bStep2 = ([Neg (Atom "B"), Atom "A", Atom "B", Neg (Atom "A")], bRule, Atom "A")
bStep3 = ([], bRule, Atom "A")
bStep4 = ([Atom "B", Neg (Atom "A")], bRule, Atom "A")
bStep5 = ([And (Atom "A") (Atom "B"), Neg (Atom "A")], bRule, Atom "A")

aConcept = And (Atom "A") (Atom "B")
aStep1 = ([aConcept], aRule, aConcept)
aStep2 = ([aConcept, Neg (Atom "A")], aRule, aConcept)
aStep3 = ([aConcept, Atom "A"], aRule, aConcept)
aStep4 = ([], aRule, aConcept)
aStep5 = ([Atom "A", And (Atom "B") (Atom "C"), Or (Atom "D") (Atom "E"),
           Neg (Atom "A"), Exists "R" (Atom "C")], aRule, aConcept)

oConcept = Or (Atom "C") (Atom "D")
oStep1 = ([oConcept], oRule, oConcept)
oStep2 = ([Atom "A", And (Atom "B") (Atom "C"), Or (Atom "D") (Atom "E"),
           Neg (Atom "A"), Exists "R" (Atom "C"), oConcept], oRule, oConcept)
oStep3 = ([], oRule, oConcept)
oStep4 = ([Atom "A", And (Atom "B") (Atom "C"), Or (Atom "D") (Atom "E"),
           Neg (Atom "A"), Exists "R" (Atom "C")], oRule, oConcept)

eConcept = Exists "R" (Atom "C")
eStep1 = ([eConcept], eRule, eConcept)
eStep2 = ([Exists "R" T], eRule, Exists "R" T)
eStep3 = ([Atom "A", aConcept, oConcept, Neg (Atom "A"), Exists "R" (Atom "A"),
           Forall "S" (Atom "E"), Forall "R" (Atom "B"),
           Forall "R" (Neg (Atom "D")), eConcept], eRule, eConcept)
eStep4 = ([], eRule, eConcept)
eStep5 = ([Atom "A", aConcept, oConcept, Neg (Atom "A"), Exists "R" (Atom "A"),
           Forall "R" (Atom "B")], eRule, eConcept)

-- Expected results & test cases of ProofSteps
bottomCorrect = ("", True, [[Neg T]])
bNotExist = "Atom A and Neg Atom A do not exist"
bottom3exp = (bNotExist, False, [[]])
bottom4exp = (bNotExist, False, [[Atom "B", Neg (Atom "A")]])
bottom5exp = (bNotExist, False, [[And (Atom "A") (Atom "B"), Neg (Atom "A")]])

btest1 = TestCase (assertEqual "Bottom rule on [Neg A, A],"
                   bottomCorrect (checkProofStep bStep1 []))
btest2 = TestCase (assertEqual "Bottom rule on [Neg B, A, B, Neg A],"
                   bottomCorrect (checkProofStep bStep2 []))
btest3 = TestCase (assertEqual "Bottom rule on [],"
                   bottom3exp (checkProofStep bStep3 []))
btest4 = TestCase (assertEqual "Bottom rule on [B, Neg A],"
                   bottom4exp (checkProofStep bStep4 []))
btest5 = TestCase (assertEqual "Bottom rule on [A and B, Neg A],"
                   bottom5exp (checkProofStep bStep5 []))

aNotExist = "And Atom A Atom B does not exist"
and1exp = ("", True, [[Atom "A", Atom "B"]])
and2exp = ("", True, [[Atom "A", Atom "B", Neg (Atom "A")]])
and3exp = ("", True, [[Atom "A", Atom "B"]])
and4exp = (aNotExist, False, [[]])
and5exp = (aNotExist, False, [[Atom "A", And (Atom "B") (Atom "C"),
                               Or (Atom "D") (Atom "E"), Neg (Atom "A"),
                               Exists "R" (Atom "C")]])

atest1 = TestCase (assertEqual "And rule on [A and B]"
                   and1exp (checkProofStep aStep1 []))
atest2 = TestCase (assertEqual "And rule on [A and B, Neg A]"
                   and2exp (checkProofStep aStep2 []))
atest3 = TestCase (assertEqual "And rule on [A and B, A]"
                   and3exp (checkProofStep aStep3 []))
atest4 = TestCase (assertEqual "And rule on []"
                   and4exp (checkProofStep aStep4 []))
atest5 = TestCase (assertEqual "And rule on [A, B and C, D or E, Neg A, ER.C]" and5exp (checkProofStep aStep5 []))

oNotExist = "Or Atom A Atom B does not exist"
or1exp = ("", True, [[Atom "C"], [Atom "D"]])
or2exp = ("", True, [[Atom "C", Atom "A", And (Atom "B") (Atom "C"),
                      Or (Atom "D") (Atom "E"), Neg (Atom "A"),
                      Exists "R" (Atom "C")],
                     [Atom "D", Atom "A", And (Atom "B") (Atom "C"),
                      Or (Atom "D") (Atom "E"), Neg (Atom "A"),
                      Exists "R" (Atom "C")]])
or3exp = (oNotExist, False, [[]])
or4exp = (oNotExist, False, [[Atom "A", And (Atom "B") (Atom "C"),
                              Or (Atom "D") (Atom "E"), Neg (Atom "A"), 
                              Exists "R" (Atom "C")]])

otest1 = TestCase (assertEqual "Or rule on [A or B]"
                   or1exp (checkProofStep oStep1 []))
otest2 = TestCase (assertEqual "Or rule on [A or B, Neg A]"
                   or2exp (checkProofStep oStep2 []))
otest3 = TestCase (assertEqual "Or rule on []"
                   or3exp (checkProofStep oStep3 []))
otest4 = TestCase (assertEqual "Or rule on [A, B and C, D or E, Neg A, ER.C]"
                   or4exp (checkProofStep oStep4 []))

eNotExist = "Exists R (Atom C) does not exist"
exists1exp = ("", True, [[Atom "C"]])
exists2exp = ("", True, [[T]])
exists3exp = ("", True, [[Atom "C", Atom "B", Neg (Atom "D")]])
exists4exp = (eNotExist, False, [[]])
exists5exp = (eNotExist, False, [[Atom "A", aConcept, oConcept,
                                  Neg (Atom "A"), Exists "R" (Atom "A"),
                                  Forall "R" (Atom "B")]])
								  
etest1 = TestCase (assertEqual "Exists rule on [Exists R.C]"
                   exists1exp (checkProofStep eStep1 []))
etest2 = TestCase (assertEqual "Exists rule on [Exists R.T]"
                   exists2exp (checkProofStep eStep2 []))
etest3 = TestCase (assertEqual "Exists rule on [A, A and B, C or D, Neg A, ER.A, FS.E, FR.B, FR.Neg D, ER.C]"
                   exists3exp (checkProofStep eStep3 []))
etest4 = TestCase (assertEqual "Or rule on []"
                   exists4exp (checkProofStep eStep4 []))
etest5 = TestCase (assertEqual "Or rule on [A, B and C, D or E, Neg A, ER.A, FR.B]"
                   exists5exp (checkProofStep eStep5 []))

-- Simple checkProofStep tests
bottomtests = TestList [TestLabel "bottomtest1" btest1,
                        TestLabel "bottomtest2" btest2,
                        TestLabel "bottomtest3" btest3,
                        TestLabel "bottomtest4" btest4,
                        TestLabel "bottomtest5" btest5]

andtests = TestList [TestLabel "andtest1" atest1,
                     TestLabel "andtest2" atest2,
                     TestLabel "andtest3" atest3,
                     TestLabel "andtest4" atest4,
                     TestLabel "andtest5" atest5]

ortests = TestList [TestLabel "ortest1" otest1,
                    TestLabel "ortest2" otest2,
                    TestLabel "ortest3" otest3,
                    TestLabel "ortest4" otest4]

existstests = TestList [TestLabel "existstest1" etest1,
                        TestLabel "existstest2" etest2,
                        TestLabel "existstest3" etest3,
                        TestLabel "existstest4" etest4,
                        TestLabel "existstest4" etest5]


-- Test proofTree structures for checkProof:
leaf = NodeZero (Neg T)
bTree = NodeOne ([Atom "A", Neg (Atom "A")], bRule, Atom "A") leaf
aTree = NodeOne ([aConcept, Neg (Atom "A")], aRule, aConcept)
		(NodeOne ([Atom "A", Atom "B", Neg (Atom "A")], bRule, Atom "A") leaf)
oTree = NodeTwo ([oConcept, Neg (Atom "C"), Neg (Atom "D")], oRule, oConcept)
		(NodeOne ([Atom "C", Neg (Atom "C"), Neg (Atom "D")], bRule, Atom "C") leaf)
		(NodeOne ([Atom "D", Neg (Atom "C"), Neg (Atom "D")], bRule, Atom "D") leaf)
eTree = NodeOne ([Forall "R" (Neg (Atom "A")), eConcept, Forall "R" (Atom "A"),
                 Forall "S" (Atom "B")], eRule, eConcept)
		(NodeOne ([Atom "A", Atom "C", Neg (Atom "A")], bRule, Atom "A") leaf)

-- More complex tree structures to test:

-- Erroneous proof trees to test
badleaf = NodeZero (Neg (Atom "A"))
badbTree1 = NodeOne bStep4 leaf
badbTree2 = NodeOne bStep5 leaf
badaTree1 = NodeOne aStep1 (NodeZero (Atom "A"))
badaTree2 = NodeOne aStep5 leaf
badoTree1 = NodeTwo oStep1 (NodeZero (Atom "B")) (NodeZero (Atom "C"))
badoTree2 = NodeTwo oStep4 leaf leaf



-- Tests for conceptEquals
-- Note: A AND B != B AND A, A OR B != B OR A (not same concepts - logically equivalent)
conceptEqualstest1 = TestCase(assertBool "conceptEquals 2 empty lists" (not (conceptEquals [] [])))
conceptEqualstest2 = TestCase(assertBool "conceptEquals 1 empty list1" (not (conceptEquals [Neg T] [])))
conceptEqualstest3 = TestCase(assertBool "conceptEquals 1 empty list2" (not(conceptEquals [] [Neg T])))
conceptEqualstest4 = TestCase(assertBool "conceptEquals unequal lists1" (not ((conceptEquals [Neg T, Atom "A"] [Neg T]))))
conceptEqualstest5 = TestCase(assertBool "conceptEquals unequal lists2" (not (conceptEquals [Neg T] [Neg T, Atom "A"])))
conceptEqualstest6 = TestCase(assertBool "conceptEquals same order"
			      (conceptEquals [Neg T, Atom "A", aConcept, oConcept, eConcept,
					      Forall "R" (Atom "A")]
					     [Neg T, Atom "A", aConcept, oConcept, eConcept,
					      Forall "R" (Atom "A")]))
conceptEqualstest7 = TestCase(assertBool "conceptEquals different order"
			      (conceptEquals [Neg T, Atom "A", aConcept, oConcept, eConcept,
					      Forall "R" (Atom "A")]
					     [oConcept, Forall "R" (Atom "A"), eConcept,
					      Neg T, Atom "A", aConcept]))

conceptEqualsTests = TestList [TestLabel "conceptEquals [] []" conceptEqualstest1,
			       TestLabel "conceptEquals [Neg T] []" conceptEqualstest2,
			       TestLabel "conceptEquals [] [Neg T]" conceptEqualstest3,
			       TestLabel "conceptEquals unequal lists1" conceptEqualstest4,
			       TestLabel "conceptEquals unequal lists2" conceptEqualstest5,
			       TestLabel "conceptEquals same order" conceptEqualstest6,
			       TestLabel "conceptEquals different order" conceptEqualstest7]

-- Tests for getConcepts
getConceptsltest1 = TestCase(assertEqual "getConcept from a Leaf node" [Neg T] (getConcepts leaf))
getConceptsbtest2 = TestCase(assertEqual "getConcept from a bTree"
			        [Atom "A", Neg (Atom "A")] (getConcepts bTree))
getConceptsatest3 = TestCase(assertEqual "getConcept from a aTree"
			        [And  (Atom "A") (Atom "B"), Neg (Atom "A")] (getConcepts aTree))
getConceptsotest4 = TestCase(assertEqual "getConcept from a oTree"
			        [Or (Atom "C") (Atom "D"), Neg (Atom "C"), Neg (Atom "D")]
                    (getConcepts oTree))
getConceptsetest5 = TestCase(assertEqual "getConcept from a eTree"
			        [Neg (Atom "A"), eConcept, Forall "R" (Atom "A"),
                     Forall "S" (Atom "B")]
			        (getConcepts eTree))

getConceptTests = TestList [TestLabel "getConcepts Leaf" getConceptsltest1,
			    TestLabel "getConcepts btree" getConceptsbtest2,
			    TestLabel "getConcepts atree" getConceptsatest3,
			    TestLabel "getConcepts otree" getConceptsotest4,
			    TestLabel "getConcepts etree" getConceptsetest5]


-- Tests for checkProof
leaftest = TestCase (assertEqual "leaf tree checkProof test" ("", True)
                     (checkProof leaf []))
btreetest = TestCase (assertEqual "bottom tree checkProof test" ("", True)
                      (checkProof bTree []))
atreetest = TestCase (assertEqual "and tree checkProof test" ("", True)
                      (checkProof aTree []))
otreetest = TestCase (assertEqual "or tree checkProof test" ("", True)
                      (checkProof oTree []))
etreetest = TestCase (assertEqual "exists checkProof test" ("", True)
                      (checkProof eTree []))

badleaftest = TestCase (assertEqual "leaf tree checkProof test" ("", False)
                     (checkProof badleaf []))
badbtreetest1 = TestCase (assertEqual "bottom tree checkProof test" ("", False)
                      (checkProof badbTree1 []))
badbtreetest2 = TestCase (assertEqual "bottom tree checkProof test" ("", False)
                      (checkProof badbTree2 []))
badatreetest1 = TestCase (assertEqual "and tree checkProof test" ("", False)
                      (checkProof badaTree1 []))
badatreetest2 = TestCase (assertEqual "and tree checkProof test" ("", False)
                      (checkProof badaTree2 []))
badotreetest1 = TestCase (assertEqual "or tree checkProof test" ("", False)
                      (checkProof badoTree1 []))
badotreetest2 = TestCase (assertEqual "or tree checkProof test" ("", False)
                      (checkProof badoTree2 []))


simpletests = TestList [TestLabel "leaftest" leaftest,
                        TestLabel "btreetest" btreetest,
                        TestLabel "atreetest" atreetest,
                        TestLabel "otreetest" otreetest,
                        TestLabel "etreetest" etreetest]

						
erroneoustests = TestList [TestLabel "leaftest" badleaftest,
                         TestLabel "btreetest1" badbtreetest1,
                         TestLabel "btreetest2" badbtreetest2,
                         TestLabel "atreetest1" badatreetest1,
                         TestLabel "atreetest2" badatreetest2,
                         TestLabel "otreetest1" badotreetest1,
                         TestLabel "otreetest2" badotreetest2]
--                         TestLabel "etreetest" etreetest]

--complextests = 



-- Tests with gamma: concepts in gamma must be valid at all steps of proof
-- Root node of tree is concatennated with gamma in the start
-- Only applicable for the Exists rule
gamma = [Atom "S", Neg (Atom "T"), And (Atom "U") (Atom "V"), Or (Atom "W") (Atom "X"), Exists "R" (Atom "Y"), Forall "R" (Atom "Z")]





testgetConcepts = do runTestTT getConceptTests
                     runTestTT conceptEqualsTests

testcheckProof = do runTestTT simpletests
                    runTestTT erroneoustests

testcheckProofStep = do runTestTT bottomtests
                        runTestTT andtests
                        runTestTT ortests
                        runTestTT existstests
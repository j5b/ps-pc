{- 
   Author: Ka Wai Cheng
   Maintainer: Ka Wai Cheng
   Email: <mail.kawai.cheng@gmail.com>
   License: GPL 3.0
   File: ProofChecker_test.hs
   Description: tests for ProofChecker.hs
-}

import Signature
import Proof
import ProofChecker
import Test.HUnit

bRule = "Bottom"
aRule = "And"
oRule = "Or"
eRule = "Exists"

bConcept, aConcept, oConcept, eConcept, fConcept :: Concept
bConcept = Neg (Atom "A")
aConcept = And (Atom "A") (Atom "B")
oConcept = Or (Atom "C") (Atom "D")
eConcept = Exists "R" (Atom "E")
fConcept = Forall "R" (Atom "F")

manyConcepts, gamma :: [Concept]
manyConcepts = [bConcept, aConcept, oConcept, eConcept, fConcept]
gamma = [Atom "U", Neg (Atom "U"), And (Atom "V") (Atom "W"), Or (Atom "X") (Atom "Y"), Forall "R" (Atom "Z")]

-- With gamma
tree :: ProofTree
tree = NodeOne (manyConcepts ++ gamma , eRule, eConcept)
               (NodeTwo ([Atom "E", Atom "F", Atom "Z"] ++ gamma, oRule,
                         Or (Atom "X") (Atom "Y"))
                        (NodeOne ([Atom "X", Atom "E", Atom "F", Atom "Z", Atom "U",
                                   Neg (Atom "U"), And (Atom "V") (Atom "W"),
                                   Forall "R" (Atom "Z")], bRule, Atom "U")
                                 (NodeZero (Neg T)))
                        (NodeOne ([Atom "Y", Atom "E", Atom "F", Atom "Z", Atom "U",
                                   Neg (Atom "U"), And (Atom "V") (Atom "W"),
                                   Forall "R" (Atom "Z")], aRule,
                                  And (Atom "V") (Atom "W"))
                                 (NodeOne ([Atom "V", Atom "W", Atom "Y", Atom "E",
                                            Atom "F", Atom "Z", Atom "U",
                                            Neg (Atom "U"), Forall "R" (Atom "Z")],
                                            bRule, Atom "U")
                                          (NodeZero (Neg T)))))

-- Tests for checkProofStep
btest1 = TestCase (assertEqual "Corret bottom rule proof step"
                   ("", True, [[Neg T]])
                   (checkProofStep ([Atom "A", bConcept], bRule, Atom "A") []))
btest2 = TestCase (assertEqual "Incorrect bottom rule proof step"
                   ("", False, [[Neg (Atom "B"), Atom "A"]])
                   (checkProofStep ([Neg (Atom "B"), Atom "A"], bRule, Atom "A")
                    gamma))
btest3 = TestCase (assertEqual "Corret bottom rule proof step with gamma"
                   ("", True, [[Neg T]])
                   (checkProofStep ([Atom "A", bConcept], bRule, Atom "A") gamma))

bottomStepTests = TestList [TestLabel "bottomtest1" btest1,
                            TestLabel "bottomtest2" btest2,
                            TestLabel "bottomtest3" btest3]

atest1 = TestCase (assertEqual "Correct and rule proof step"
                   ("", True, [[Atom "A", Atom "B"]])
                   (checkProofStep ([aConcept], aRule, aConcept) []))
atest2 = TestCase (assertEqual "Incorrect and rule proof step"
                   ("", False, [[aConcept]])
                   (checkProofStep ([aConcept], aRule, And (Atom "A") (Atom "C"))
                    gamma))
atest3 = TestCase (assertEqual "Correct and rule proof step with gamma"
                   ("", True, [[Atom "A", Atom "B"]])
                   (checkProofStep ([aConcept], aRule, aConcept) gamma))

andStepTests = TestList [TestLabel "andtest1" atest1,
                         TestLabel "andtest2" atest2,
                         TestLabel "andtest3" atest3]

otest1 = TestCase (assertEqual "Corret or rule proof step"
                   ("", True, [[Atom "C", bConcept], [Atom "D", bConcept]])
                   (checkProofStep ([oConcept, bConcept], oRule, oConcept) []))
otest2 = TestCase (assertEqual "Incorrect or rule proof step"
                   ("", False, [[oConcept]])
                   (checkProofStep ([oConcept], oRule, Or (Atom "A") (Atom "B"))
                    gamma))
otest3 = TestCase (assertEqual "Corret or rule proof step with gamma"
                   ("", True, [[Atom "C", bConcept], [Atom "D", bConcept]])
                   (checkProofStep ([oConcept, bConcept], oRule, oConcept) gamma))

orStepTests = TestList [TestLabel "ortest1" btest1,
                        TestLabel "ortest2" btest2,
                        TestLabel "ortest3" btest3]

etest1 = TestCase (assertEqual "Correct exists rule proof step"
                   ("", True, [[Atom "E", Atom "F"]])
                   (checkProofStep ([eConcept, fConcept, Forall "S" (Atom "G")],
                    eRule, eConcept) []))
etest2 = TestCase (assertEqual "Incorrect exists rule proof step"
                   ("", False, [[eConcept]])
                   (checkProofStep ([eConcept], eRule, Exists "R" (Atom "A")) gamma))
etest3 = TestCase (assertEqual "Correct exists rule proof step with gamma"
                   ("", True, [[Atom "E", Atom "F"] ++ gamma])
                   (checkProofStep ([eConcept, fConcept, Forall "S" (Atom "G")],
                    eRule, eConcept) gamma))

existsStepTests = TestList [TestLabel "existstest1" etest1,
                            TestLabel "existstest2" etest2,
                            TestLabel "existstest3" etest3]

testcheckProofStep = do runTestTT bottomStepTests
                        runTestTT andStepTests
                        runTestTT orStepTests
                        runTestTT existsStepTests


-- Tests for checkProof
leaftest1 = TestCase (assertEqual "Correct leaf" ("", True)
                      (checkProof (NodeZero (Neg T)) []))
leaftest2 = TestCase (assertEqual "Incorrect leaf" ("", False)
                      (checkProof (NodeZero (Atom "A")) gamma))
leaftest3 = TestCase (assertEqual "Correct leaf with gamma" ("", True)
                      (checkProof (NodeZero (Neg T)) gamma))

leafTests = TestList [TestLabel "leaftest1" leaftest1,
                      TestLabel "leaftest2" leaftest2,
                      TestLabel "leaftest3" leaftest3]

treetest1 = TestCase (assertEqual "Correct tree" ("", True)
                      (checkProof tree gamma))
treetest2 = TestCase (assertEqual "Incorrect proof step" ("", False)
                      (checkProof (NodeOne ([Neg (Atom "B"), Atom "A"], bRule,
                                            Atom "A")
                                           (NodeZero (Neg T))) gamma))

treeTests = TestList [TestLabel "treetest1" treetest1,
                      TestLabel "treetest2" treetest2]

testcheckProof = do runTestTT leafTests
                    runTestTT treeTests

-- Tests for ProofChecker
prooftest1 = TestCase (assertEqual "Correct proof" ("", True) 
                       (proofChecker tree gamma))

proofCheckerTests = TestList [TestLabel "prooftreetest1" prooftest1]

testProofChecker = do runTestTT proofCheckerTests
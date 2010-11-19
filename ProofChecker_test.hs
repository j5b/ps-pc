{- 
   Author: Ka Wai Cheng
   Maintainer: Ka Wai Cheng
   Email: <mail.kawai.cheng@gmail.com>
   License: GPL 3.0
   File: ProofChecker_test.hs
   Description: tests for ProofChecker.hs
-}

module ProofChecker_test where 

import Signature
import Proof
import ProofChecker
import Test.HUnit

bRule = "bottom"
aRule = "and"
oRule = "or"
eRule = "exists"

bConcept, aConcept, oConcept, eConcept, fConcept :: Concept
bConcept = Neg (Atom "A")
aConcept = And (Atom "A") (Atom "B")
oConcept = Or (Atom "C") (Atom "D")
eConcept = Exists "R" (Atom "E")
fConcept = Forall "R" (Atom "F")

manyConcepts, gamma :: [Concept]
manyConcepts = [bConcept, aConcept, oConcept, eConcept, fConcept]
gamma = [Atom "U", Neg (Atom "U"), And (Atom "V") (Atom "W"),
         Or (Atom "X") (Atom "Y"), Forall "R" (Atom "Z")]

bTree = NodeZero ([Atom "A", Neg (Atom "A")], bRule, Atom "A")
aTree = NodeOne ([aConcept, Neg (Atom "A")], aRule, aConcept)
		(NodeZero ([Atom "A", Atom "B", Neg (Atom "A")], bRule, Atom "A"))
oTree = NodeTwo ([oConcept, Neg (Atom "C"), Neg (Atom "D")], oRule, oConcept)
		(NodeZero ([Atom "C", Neg (Atom "C"), Neg (Atom "D")], bRule, Atom "C"))
		(NodeZero ([Atom "D", Neg (Atom "C"), Neg (Atom "D")], bRule, Atom "D"))
eTree1 = NodeOne ([Forall "R" (Neg (Atom "A")), eConcept, Forall "R" (Atom "A"),
                  Forall "S" (Atom "B")], eRule, eConcept)
         (NodeZero ([Atom "A", Atom "E", Neg (Atom "A")], bRule, Atom "A"))
-- etree with gamma
eTree2 = NodeOne ([Forall "R" (Neg (Atom "A")), eConcept, Forall "R" (Atom "A"),
                  Forall "S" (Atom "B")], eRule, eConcept)
         (NodeZero ([Atom "A", Atom "E", Neg (Atom "A"), Atom "N"],
          bRule, Atom "A"))

-- With gamma
tree :: ProofTree
tree = NodeOne (manyConcepts ++ gamma , eRule, eConcept)
               (NodeTwo ([Atom "E", Atom "F", Atom "Z"] ++ gamma, oRule,
                         Or (Atom "X") (Atom "Y"))
                        (NodeZero ([Atom "X", Atom "E", Atom "F", Atom "Z", Atom "U",
                                   Neg (Atom "U"), And (Atom "V") (Atom "W"),
                                   Forall "R" (Atom "Z")], bRule, Atom "U"))
                        (NodeOne ([Atom "Y", Atom "E", Atom "F", Atom "Z", Atom "U",
                                   Neg (Atom "U"), And (Atom "V") (Atom "W"),
                                   Forall "R" (Atom "Z")], aRule,
                                  And (Atom "V") (Atom "W"))
                                 (NodeZero ([Atom "V", Atom "W", Atom "Y", Atom "E",
                                            Atom "F", Atom "Z", Atom "U",
                                            Neg (Atom "U"), Forall "R" (Atom "Z")],
                                            bRule, Atom "U"))))



-- Tests for checkProofStep
btest1 = TestCase (assertEqual "Correct bottom rule proof step"
                   ("", True, [])
                   $ checkProofStep ([Atom "A", bConcept], bRule, Atom "A") [])
btest2 = TestCase (assertEqual "InCorrect bottom rule proof step"
                   ("Atom A and Not (Atom A) do not both exist in the set of"
                    ++ " concepts {Not (Atom B), Atom A}", False,
                    [[Neg (Atom "B"), Atom "A"]])
                   $ checkProofStep ([Neg (Atom "B"), Atom "A"], bRule, Atom "A")
                    gamma)
btest3 = TestCase (assertEqual "Correct bottom rule proof step with gamma"
                   ("", True, [])
                   $ checkProofStep ([Atom "A", bConcept], bRule, Atom "A") gamma)

bottomStepTests = TestList [TestLabel "bottomtest1" btest1,
                            TestLabel "bottomtest2" btest2,
                            TestLabel "bottomtest3" btest3]

atest1 = TestCase (assertEqual "Correct and rule proof step"
                   ("", True, [[Atom "A", Atom "B"]])
                   $ checkProofStep ([aConcept], aRule, aConcept) [])
atest2 = TestCase (assertEqual "InCorrect and rule proof step"
                   ("(Atom A and Atom C) does not exist in the set of " ++
                    "concepts {(Atom A and Atom B)}", False, [[aConcept]])
                   $ checkProofStep ([aConcept], aRule, And (Atom "A") (Atom "C"))
                    gamma)
atest3 = TestCase (assertEqual "Correct and rule proof step with gamma"
                   ("", True, [[Atom "A", Atom "B"]])
                   $ checkProofStep ([aConcept], aRule, aConcept) gamma)

andStepTests = TestList [TestLabel "andtest1" atest1,
                         TestLabel "andtest2" atest2,
                         TestLabel "andtest3" atest3]

otest1 = TestCase (assertEqual "Correct or rule proof step"
                   ("", True, [[Atom "C", bConcept], [Atom "D", bConcept]])
                   $ checkProofStep ([oConcept, bConcept], oRule, oConcept) [])
otest2 = TestCase (assertEqual "InCorrect or rule proof step"
                   ("(Atom A or Atom B) does not exist in the set of " ++
                    "concepts {(Atom C or Atom D)}", False, [[oConcept]])
                   $ checkProofStep ([oConcept], oRule, Or (Atom "A") (Atom "B"))
                    gamma)
otest3 = TestCase (assertEqual "Correct or rule proof step with gamma"
                   ("", True, [[Atom "C", bConcept], [Atom "D", bConcept]])
                   $ checkProofStep ([oConcept, bConcept], oRule, oConcept) gamma)

orStepTests = TestList [TestLabel "ortest1" otest1,
                        TestLabel "ortest2" otest2,
                        TestLabel "ortest3" otest3]

etest1 = TestCase (assertEqual "Correct exists rule proof step"
                   ("", True, [[Atom "E", Atom "F"]])
                   $ checkProofStep ([eConcept, fConcept, Forall "S" (Atom "G")],
                    eRule, eConcept) [])
etest2 = TestCase (assertEqual "InCorrect exists rule proof step"
                   ("(Exists R Atom A) does not exist in the set of " ++
                    "concepts {(Exists R Atom E)}", False, [[eConcept]])
                   $ checkProofStep ([eConcept], eRule, Exists "R" (Atom "A")) gamma)
etest3 = TestCase (assertEqual "Correct exists rule proof step with gamma"
                   ("", True, [[Atom "E", Atom "F"] ++ gamma])
                   $ checkProofStep ([eConcept, fConcept, Forall "S" (Atom "G")],
                    eRule, eConcept) gamma)

existsStepTests = TestList [TestLabel "existstest1" etest1,
                            TestLabel "existstest2" etest2,
                            TestLabel "existstest3" etest3]

testcheckProofStep = do runTestTT bottomStepTests
                        runTestTT andStepTests
                        runTestTT orStepTests
                        runTestTT existsStepTests


-- Tests for checkTree
leaftest1 = TestCase (assertEqual "Correct leaf" ("", True)
                      $ checkTree (NodeZero ([Neg T], "", Neg T)) [])
leaftest2 = TestCase (assertEqual "InCorrect leaf"
                      ("This proof tree does not show unsatisfiability", False)
                      $ checkTree (NodeZero ([Atom "A"], "", Atom "A")) gamma)
leaftest3 = TestCase (assertEqual "Uncomplete end of proof"
                      ("Proof tree is not complete, applying the and rule to {"
                       ++ "(Atom A and Atom B)} produces {Atom A, Atom B}", False)
                      $ checkTree (NodeZero ([aConcept], aRule, aConcept)) gamma)
leaftest4 = TestCase (assertEqual "Correct leaf with gamma" ("", True)
                      $ checkTree (NodeZero ([Neg T], "", Neg T)) gamma)

leafTests = TestList [TestLabel "leaftest1" leaftest1,
                      TestLabel "leaftest2" leaftest2,
                      TestLabel "leaftest3" leaftest3,
                      TestLabel "leaftest4" leaftest4]

btreetest1 = TestCase (assertEqual "Correct simple bottom rule tree" ("", True)
                       $ checkTree bTree [])
btreetest2 = TestCase (assertEqual "Incorrect simple bottom rule tree"
                       ("There should be 1 resulting set of concepts for " ++
                        "NodeOne", False) $ checkTree
                       (NodeOne ([bConcept, Atom "A"], bRule, Atom "A")
                                (NodeZero ([bottom], "", bottom))) [])
atreetest1 = TestCase (assertEqual "Correct simple and rule tree" ("", True)
                       $ checkTree aTree [])
atreetest2 = TestCase (assertEqual "Incorrect simple and rule tree"
                       ("Next step's concepts (Atom A, Not (Atom B)) do " ++
                        "not match the result of applying and rule to ((" ++
                        "Atom A and Atom B), Not (Atom A))", False) $ checkTree
                        (NodeOne ([aConcept, bConcept], aRule, aConcept)
                        (NodeZero ([Atom "A", Neg (Atom "B")], bRule, Atom "B")
                        )) [])
otreetest1 = TestCase (assertEqual "Correct simple or rule tree" ("", True)
                       $ checkTree oTree [])
otreetest2 = TestCase (assertEqual "Incorrect simple or rule tree"
                      ("Applying the and rule to {(Atom A and Atom B), " ++
                       "Not (Atom A)} should not give 2 results", False)
                      $ checkTree (NodeTwo ([aConcept, bConcept], aRule, aConcept)
                      (NodeZero ([Atom "A", bConcept], bRule, Atom "A"))
                      (NodeZero ([Atom "A", bConcept], bRule, Atom "A"))) [])
etreetest1 = TestCase (assertEqual "Correct simple exists rule tree" ("", True)
                       $ checkTree eTree1 [])

etreetest2 = TestCase (assertEqual "Correct simple exists rule tree2" ("", True)
                       $ checkTree eTree2 [Atom "N"])

simpleTreeTests = TestList [TestLabel "simplebtreetest1" btreetest1,
                            TestLabel "simplebtreetest2" btreetest2,
                            TestLabel "simpleatreetest1" atreetest1,
                            TestLabel "simpleatreetest2" atreetest2,
                            TestLabel "simpleotreetest1" otreetest1,                            TestLabel "simpleotreetest2" otreetest2,
                            TestLabel "simpleetreetest1" etreetest1,                            TestLabel "simpleetreetest2" etreetest2]

treetest1 = TestCase (assertEqual "Correct tree" ("", True)
                      $ checkTree tree gamma)
treetest2 = TestCase (assertEqual "InCorrect proof step"
                      ("Atom A and Not (Atom A) do not both exist in the " ++
                       "set of concepts {Not (Atom B), Atom A}", False)
                      $ checkTree (NodeZero ([Neg (Atom "B"), Atom "A"], bRule,
                                             Atom "A")) gamma)

treeTests = TestList [TestLabel "treetest1" treetest1,
                      TestLabel "treetest2" treetest2]

testcheckTree = do runTestTT leafTests
                   runTestTT simpleTreeTests
                   runTestTT treeTests


-- Tests for checkProof
prooftest1 = TestCase (assertEqual "Correct proof" ("", True) 
                       $ checkProof tree gamma)
prooftest2 = TestCase (assertEqual "Duplicates in concepts proof"
                       ("Initial concepts must not contain duplicate concepts",
                        False) $ checkProof (NodeZero
                       ([aConcept, aConcept], "", aConcept)) [Atom "A"])
prooftest3 = TestCase (assertEqual "Duplicates in gamma proof"
                       ("Gamma must not contain duplicate concepts", False)
                       $ checkProof tree [Atom "A", Atom "A"])
prooftest4 = TestCase (assertEqual "Initial concepts not NNF in proof"
                       ("Initial concepts are not in negation normal form",
                        False) $ checkProof (NodeZero
                       ([Neg T, Neg aConcept], "", Neg T)) gamma)
prooftest5 = TestCase (assertEqual "Concepts in gamma not NNF in proof"
                       ("Concepts in gamma are not in negation normal form",
                        False) $ checkProof tree [Neg aConcept])
prooftest6 = TestCase (assertEqual "Gamma not in initial concepts proof"
                       ("Concepts in gamma are not in the initial set of " ++
                        "concepts", False) $ checkProof tree [Atom "P"])

checkProofTests = TestList [TestLabel "prooftreetest1" prooftest1,
                            TestLabel "prooftreetest2" prooftest2,
                            TestLabel "prooftreetest3" prooftest3,
                            TestLabel "prooftreetest4" prooftest4,
                            TestLabel "prooftreetest5" prooftest5,
                            TestLabel "prooftreetest6" prooftest6]

testcheckProof = runTestTT checkProofTests

proofCheckerTests = do putStrLn "==== Proof Checker tests"
                       testcheckProofStep
                       testcheckTree
                       testcheckProof
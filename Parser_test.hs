{- 
   Author: Ka Wai, Jannis
   License: GPL 3.0
   File: Parser_test.hs
   Description: tests for the parser Parser.y/hs
-}

module Parser_test where

import System.IO
import Signature
import Parser
import TestUtils
import Test.HUnit

allparsertests = do
  allIbasictests
  allb1basictests
  b1parsefiletests
  allb2basictests
  b2parsefiletests

iTopBotTests = maplabel "Input parse top/bot test" [itoptest, ibottest]
iNotTests    = maplabel "Input parse not test"     [inottest1, inottest2]
iAndTests    = maplabel "Input parse and test"     [iandtest1, iandtest2]
iOrTests     = maplabel "Input parse or test"      [iortest1, iortest2]
iImpTests    = maplabel "Input parse implies test" [iimptest1, iimptest2]
iForallTests = maplabel "Input parse forall test"  [iforalltest1, iforalltest2]
iExistsTests = maplabel "Input parse exists test"  [iexiststest1, iexiststest2]

allIbasictests = do putStrLn "==== Testing the parser for input concepts"
                    runTestTT iTopBotTests
                    runTestTT iNotTests
                    runTestTT iAndTests
                    runTestTT iOrTests
                    runTestTT iImpTests
                    runTestTT iForallTests
                    runTestTT iExistsTests

b1NotTests = maplabel "Benchmark 1 parse not test" [b1nottest1, b1nottest2]
b1AndTests = maplabel "Benchmark 1 parse and test" [b1andtest1, b1andtest2]
b1ImpTests = maplabel "Benchmark 1 parse implies test" [b1imptest1, b1imptest2]
b1BoxTests = maplabel "Benchmark 1 parse box test" [b1boxtest1, b1boxtest2]
b1DiaTests = maplabel "Benchmark 1 parse dia test" [b1diatest1, b1diatest2]

allb1basictests = do putStrLn "==== Testing the parser for benchmark 1 concepts"
                     runTestTT b1NotTests
                     runTestTT b1AndTests
                     runTestTT b1ImpTests
                     runTestTT b1BoxTests
                     runTestTT b1DiaTests

b1FileTests = maplabel "Benchmark 1 parse file test" [b1test1, b1test2]

b1parsefiletests = do putStrLn "==== Testing the parser for benchmark 1 files"
                      runTestTT b1FileTests

b2NotTests = maplabel "Benchmark 2 parse not test" [b2nottest1, b2nottest2]
b2AndTests = maplabel "Benchmark 2 parse and test" [b2andtest1, b2andtest2]
b2OrTests  = maplabel "Benchmark 2 parse or test" [b2ortest1, b2ortest2]
b2BoxTests = maplabel "Benchmark 2 parse box test" [b2boxtest1, b2boxtest2]
b2DiaTests = maplabel "Benchmark 2 parse dia test" [b2diatest1, b2diatest2]
bTrueTests = maplabel "Benchmark 2 parse true test" [b2truetest1, b2truetest2]
b2FalseTests = maplabel "Benchmark 2 parse false test" [b2falsetest1, b2falsetest2]

allb2basictests = do putStrLn "==== Testing the parser for benchmark 2 concepts"
                     runTestTT b2NotTests
                     runTestTT b2AndTests
                     runTestTT b2OrTests
                     runTestTT b2BoxTests
                     runTestTT b2DiaTests
                     runTestTT bTrueTests
                     runTestTT b2FalseTests

b2FileTests = maplabel "Benchmark 2 parse file test" [b2test1, b2test2]

b2parsefiletests = do putStrLn "==== Testing the parser for benchmark 2 files"
                      runTestTT b2FileTests

-- Testing setup

listallI = concatMap labelMaker $ zip [1..] allIConcepts
    where labelMaker (a,b) = show a ++ ": " ++ b ++ " "

allIConcepts = [inot1, inot2, iand1, iand2, iimplies1, iimplies2, iforall1,
                iforall2, iexists1, iexists2]

inot1 = "~atom1"
inot2 = "~(~atom1)"
iand1 = "a & b"
iand2 = "((a & b) & c)"
ior1 = "a | b"
ior2 = "(a | (b | c))"
iimplies1 = "a -> b"
iimplies2 = "a -> (b -> c)"
iforall1 = "Forall R (a)"
iforall2 = "Forall R (a & b)"
iexists1 = "Exists R (a)"
iexists2 = "Exists R (a & b)"
itrue = "top"
ifalse = "bot"


listallb1 = concatMap labelMaker $ zip [1..] allb1Concepts
  where labelMaker (a,b) = show a ++ ": " ++ b ++ " "

allb1Concepts = [b1not1, b1not2, b1and1, b1and2, b1implies1, b1implies2, b1box1,
                 b1box2, b1dia2]

b1not1 = "~atom1"
b1not2 = "~(~atom1)"
b1and1 = "a & b"
b1and2 = "((a & b) & c)"
b1implies1 = "a -> b"
b1implies2 = "a -> (b -> c)"
b1box1 = "box(a)"
b1box2 = "box(a & b)"
b1dia1 = "dia(a)"
b1dia2 = "dia(a & b)"

b1file1 = "text to be ignored begin 1: " ++ b1not1 ++ " end"
b1file2 = "benchmark formulas k_branch_n.txt begin "++ listallb1 ++" end"



b2true1  = "true"
b2true2  = "or(true, false)"
b2false1 = "false"
b2false2 = "and(true, false)"
b2or1    = "or(a, b)"
b2or2    = "or(a, or(b, c))"
b2not1   = "not(atom1)"
b2not2   = "not(not(atom1))"
b2and1   = "and(a, b)"
b2and2   = "and(and(a, b), c)"
b2box1   = "box(R, a)"
b2box2   = "box(R, and(a, b))"
b2dia1   = "dia(R, a)"
b2dia2   = "dia(R, and(a, b))"

b2file1 = "begin_problem(Unknown). text to be ignored. " ++
          "list_of_special_formulae(conjectures, EML). " ++
          "prop_formula(" ++ b2not1 ++ "). end_of_list. " ++
          "This text should be ignored. " ++
          "list_of_special_formulae(conjectures, EML). " ++
          "prop_formula(" ++ b2true1 ++ "). end_of_list. end_problem."
b2file2 = "begin_problem(Unknown). text to be ignored. " ++
          "list_of_special_formulae(conjectures, EML). "++ allb2a ++
          " end_of_list. This text should be ignored. " ++
          "list_of_special_formulae(conjectures, EML). " ++ allb2b ++
          " end_of_list. This text should be ignored. end_problem."
  where allb2a = concat ["prop_formula(" ++ c ++ "). " | c <- allb2Conceptsa ]
        allb2b = concat ["prop_formula(" ++ c ++ "). " | c <- allb2Conceptsb ]

allb2Conceptsa = [b2true1, b2true2, b2false1, b2false2, b2or1, b2or2]
allb2Conceptsb = [b2not1, b2not2, b2and1, b2and2, b2box1, b2box2, b2dia1, b2dia2]
allb2Targets = concat [top1target, top2target, bot1target, bot2target,
               or1target, or2target, not1target, not2target,and1target,
               and2target, box1target, box2target, dia1target, dia2target]

-- Expected results
top1target     = [T]
top2target     = [Or T (Neg T)]
bot1target     = [Neg T]
bot2target     = [And T (Neg T)]
not1target     = [Neg (Atom "atom1")]
not2target     = [Neg(Neg (Atom "atom1"))]
and1target     = [And (Atom "a") (Atom "b")]
and2target     = [And (And (Atom "a") (Atom "b")) (Atom "c")]
implies1target = [Or (Neg (Atom "a")) (Atom "b")]
implies2target = [Or (Neg (Atom "a")) (Or (Neg(Atom "b")) (Atom "c"))]
box1target     = [Forall "R" (Atom "a")]
box2target     = [Forall "R" (And (Atom "a") (Atom "b"))]
dia1target     = [Exists "R" (Atom "a")]
dia2target     = [Exists "R" (And (Atom "a") (Atom "b"))]
or1target      = [Or (Atom "a") (Atom "b")]
or2target      = [Or (Atom "a") (Or (Atom "b") (Atom "c"))]

-- Tests for parsing input concepts

itoptest = testequality msg target result itrue
  where msg    = "Failed to correctly parse a top concept from input"
        result = file $ lexerI itrue
        target = top1target

ibottest = testequality msg target result ifalse
  where msg    = "Failed to correctly parse a bot concept from input"
        result = file $ lexerI ifalse
        target = bot1target

inottest1 = testequality msg target result inot1
  where msg    = "Failed to correctly parse a not concept from input"
        result = file $ lexerI inot1
        target = not1target

inottest2 = testequality msg target result inot2
  where msg    = "Failed to correctly parse a not concept from input"
        result = file $ lexerI inot2
        target = not2target

iandtest1 = testequality msg target result iand1
  where msg    = "Failed to correctly parse an and concept from input"
        result = file $ lexerI iand1
        target = and1target

iandtest2 = testequality msg target result iand2
  where msg    = "Failed to correctly parse an and concept from input"
        result = file $ lexerI iand2
        target = and2target

iortest1 = testequality msg target result ior1
  where msg    = "Failed to correctly parse an and concept from input"
        result = file $ lexerI ior1
        target = or1target

iortest2 = testequality msg target result ior2
  where msg    = "Failed to correctly parse an and concept from input"
        result = file $ lexerI ior2
        target = or2target

iimptest1 = testequality msg target result iimplies1
  where msg    = "Failed to correctly parse an and concept from input"
        result = file $ lexerI iimplies1
        target = implies1target

iimptest2 = testequality msg target result iimplies2
  where msg    = "Failed to correctly parse an implies concept from input"
        result = file $ lexerI iimplies2
        target = implies2target

iforalltest1 = testequality msg target result iforall1
  where msg    = "Failed to correctly parse a implies concept from input"
        result = file $ lexerI iforall1
        target = box1target

iforalltest2 = testequality msg target result iforall2
  where msg    = "Failed to correctly parse a box concept from input"
        result = file $ lexerI iforall2
        target = box2target

iexiststest1 = testequality msg target result iexists1
  where msg    = "Failed to correctly parse a dia concept from input"
        result = file $ lexerI iexists1
        target = dia1target

iexiststest2 = testequality msg target result iexists2
  where msg    = "Failed to correctly parse a dia concept from input"
        result = file $ lexerI iexists2
        target = dia2target

-- Tests for parsing benchmark 1 concepts
b1nottest1 = testequality msg target result b1not1
  where msg    = "Failed to correctly parse a not concept from Benchmark 1"
        result = file $ lexB1Concepts b1not1
        target = not1target

b1nottest2 = testequality msg target result b1not2
  where msg    = "Failed to correctly parse a not concept from Benchmark 1"
        result = file $ lexB1Concepts b1not2
        target = not2target

b1andtest1 = testequality msg target result b1and1
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexB1Concepts b1and1
        target = and1target

b1andtest2 = testequality msg target result b1and2
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexB1Concepts b1and2
        target = and2target

b1imptest1 = testequality msg target result b1implies1
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexB1Concepts b1implies1
        target = implies1target

b1imptest2 = testequality msg target result b1implies2
  where msg    = "Failed to correctly parse an implies concept from Benchmark 1"
        result = file $ lexB1Concepts b1implies2
        target = implies2target

b1boxtest1 = testequality msg target result b1box1
  where msg    = "Failed to correctly parse a implies concept from Benchmark 1"
        result = file $ lexB1Concepts b1box1
        target = box1target

b1boxtest2 = testequality msg target result b1box2
  where msg    = "Failed to correctly parse a box concept from Benchmark 1"
        result = file $ lexB1Concepts b1box2
        target = box2target

b1diatest1 = testequality msg target result b1dia1
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexB1Concepts b1dia1
        target = dia1target

b1diatest2 = testequality msg target result b1dia2
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexB1Concepts b1dia2
        target = dia2target

-- Tests for parsing benchmark 1 files
b1test1 = testequality msg target result b1file1
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1 b1file1
        target = [Neg (Atom "atom1")]

b1test2 = testequality msg target result b1file2
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1 b1file2
        target = reverse [Neg (Atom "atom1"), Neg(Neg (Atom "atom1")), And (Atom "a")
                  (Atom "b"), And (And (Atom "a") (Atom "b")) (Atom "c"),
                  Or (Neg (Atom "a")) (Atom "b"), Or (Neg (Atom "a"))
                  (Or (Neg(Atom "b")) (Atom "c")), Forall "R" (Atom "a"),
                  Forall "R" (And (Atom "a") (Atom "b")), Exists "R"
                 (And (Atom "a") (Atom "b"))]

{-
btest3 = testequality msg target result "k_branch_n.txt"
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = do input <- readFile "k_branch_n.txt"
                    file $ lexerB1 input
        target = [Neg (Atom "atom1")]
-}

-- Tests for parsing benchmark 2 concepts
b2nottest1 = testequality msg target result b2not1
  where msg    = "Failed to correctly parse a not concept from Benchmark 2"
        result = file $ lexB2Concept b2not1
        target = not1target

b2nottest2 = testequality msg target result b2not2
  where msg    = "Failed to correctly parse a not concept from Benchmark 2"
        result = file $ lexB2Concept b2not2
        target = not2target

b2andtest1 = testequality msg target result b2and1
  where msg    = "Failed to correctly parse an and concept from Benchmark 2"
        result = file $ lexB2Concept b2and1
        target = and1target

b2andtest2 = testequality msg target result b2and2
  where msg    = "Failed to correctly parse an and concept from Benchmark 2"
        result = file $ lexB2Concept b2and2
        target = and2target

b2ortest1 = testequality msg target result b2or1
  where msg    = "Failed to correctly parse an or concept from Benchmark 2"
        result = file $ lexB2Concept b2or1
        target = or1target

b2ortest2 = testequality msg target result b2or2
  where msg    = "Failed to correctly parse an or concept from Benchmark 2"
        result = file $ lexB2Concept b2or2
        target = or2target

b2boxtest1 = testequality msg target result b2box1
  where msg    = "Failed to correctly parse a box concept from Benchmark 2"
        result = file $ lexB2Concept b2box1
        target = box1target

b2boxtest2 = testequality msg target result b2box2
  where msg    = "Failed to correctly parse a box concept from Benchmark 2"
        result = file $ lexB2Concept b2box2
        target = box2target

b2diatest1 = testequality msg target result b2dia1
  where msg    = "Failed to correctly parse a dia concept from Benchmark 2"
        result = file $ lexB2Concept b2dia1
        target = dia1target

b2diatest2 = testequality msg target result b2dia2
  where msg    = "Failed to correctly parse a dia concept from Benchmark 2"
        result = file $ lexB2Concept b2dia2
        target = dia2target

b2truetest1 = testequality msg target result b2true1
  where msg    = "Failed to correctly parse a true concept from Benchmark 2"
        result = file $ lexB2Concept b2true1
        target = top1target

b2truetest2 = testequality msg target result b2true2
  where msg    = "Failed to correctly parse a true concept from Benchmark 2"
        result = file $ lexB2Concept b2true2
        target = top2target

b2falsetest1 = testequality msg target result b2false1
  where msg    = "Failed to correctly parse a false concept from Benchmark 2"
        result = file $ lexB2Concept b2false1
        target = bot1target

b2falsetest2 = testequality msg target result b2false2
  where msg    = "Failed to correctly parse a false concept from Benchmark 2"
        result = file $ lexB2Concept b2false2
        target = bot2target

-- Tests for parsing benchmark 2 files
b2test1 = testequality msg target result b2file1
  where msg    = "Failed to correctly parse a file from Benchmark 2"
        result = file $ lexerB2 b2file1
        target = top1target ++ not1target

b2test2 = testequality msg target result b2file2
  where msg    = "Failed to correctly parse a file from Benchmark 2"
        result = file $ lexerB2 b2file2
        target = reverse allb2Targets


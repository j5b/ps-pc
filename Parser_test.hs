{- 
   Author: Ka Wai
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

b1NotTests = maplabel "Benchmark 1 parse not test" [b1nottest1, b1nottest2]
b1AndTests = maplabel "Benchmark 1 parse and test" [b1andtest1, b1andtest2]
b1ImpTests = maplabel "Benchmark 1 parse implies test" [b1imptest1, b1imptest2]
b1BoxTests = maplabel "Benchmark 1 parse box test" [b1boxtest1, b1boxtest2]
b1DiaTests = maplabel "Benchmark 1 parse box test" [b1diatest1, b1diatest2]

allb1basictests = do putStrLn "==== Testing the parser for benchmark 1 concepts"
                     runTestTT b1NotTests
                     runTestTT b1AndTests
                     runTestTT b1ImpTests
                     runTestTT b1BoxTests
                     runTestTT b1DiaTests

b1FileTests = maplabel "Benchmark 1 parse file test" [b1test1, b1test2]

b1parsefiletests = do putStrLn "==== Testing the parser for benchmark 1 files"
                      runTestTT b1FileTests

-- Testing setup

listallb1 = concat $ map labelMaker $ zip [1..] allb1Concepts
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

-- Tests for parsing benchmark 1 concepts
b1nottest1 = testequality msg target result b1not1
  where msg    = "Failed to correctly parse a not concept from Benchmark 1"
        result = file $ lexerB1Concepts b1not1
        target = [Neg (Atom "atom1")]

b1nottest2 = testequality msg target result b1not2
  where msg    = "Failed to correctly parse a not concept from Benchmark 1"
        result = file $ lexerB1Concepts b1not2
        target = [Neg(Neg (Atom "atom1"))]

b1andtest1 = testequality msg target result b1and1
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexerB1Concepts b1and1
        target = [And (Atom "a") (Atom "b")]

b1andtest2 = testequality msg target result b1and2
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexerB1Concepts b1and2
        target = [And (And (Atom "a") (Atom "b")) (Atom "c")]

b1imptest1 = testequality msg target result b1implies1
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexerB1Concepts b1implies1
        target = [Or (Neg (Atom "a")) (Atom "b")]

b1imptest2 = testequality msg target result b1implies2
  where msg    = "Failed to correctly parse an and concept from Benchmark 1"
        result = file $ lexerB1Concepts b1implies2
        target = [Or (Neg (Atom "a")) (Or (Neg(Atom "b")) (Atom "c"))]

b1boxtest1 = testequality msg target result b1box1
  where msg    = "Failed to correctly parse a box concept from Benchmark 1"
        result = file $ lexerB1Concepts b1box1
        target = [Forall "R" (Atom "a")]

b1boxtest2 = testequality msg target result b1box2
  where msg    = "Failed to correctly parse a box concept from Benchmark 1"
        result = file $ lexerB1Concepts b1box2
        target = [Forall "R" (And (Atom "a") (Atom "b"))]

b1diatest1 = testequality msg target result b1dia1
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1Concepts b1dia1
        target = [Exists "R" (Atom "a")]

b1diatest2 = testequality msg target result b1dia2
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1Concepts b1dia2
        target = [Exists "R" (And (Atom "a") (Atom "b"))]

-- Tests for parsing benchmark 1 files
b1test1 = testequality msg target result b1file1
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1 b1file1
        target = [Neg (Atom "atom1")]

b1test2 = testequality msg target result b1file2
  where msg    = "Failed to correctly parse a dia concept from Benchmark 1"
        result = file $ lexerB1 b1file2
        target = [Neg (Atom "atom1"), Neg(Neg (Atom "atom1")), And (Atom "a")
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



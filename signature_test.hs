{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: signature_test.hs
   Description: Tests the signature.hs file
-}

import Signature
import Test.HUnit

testLabel (a,b) = TestLabel a b

-- simple checks constructors
simple1, simple2, simple3, simple4, simple5, simple6, simple7, simple8 :: Concept
simple1 = atom "C"
simple2 = top \/ bottom
simple3 = top /\ simple1
simple4 = neg simple1
simple5 = simple1 .> simple4
simple6 = simple1 <-> (neg simple4) -- this one looks very complicated
simple7 = exists "R" simple1
simple8 = forall "R" bottom

-- to Negation Normal Form test
nnfTTemplate msg a b =
  TestCase (assertBool msg $ a == (toNNF b))

nnfTest1 =
  nnfTTemplate "toNNF not affecting T" top top
nnfTest2 =
  nnfTTemplate "toNNF not affecting not T" bottom bottom
nnfTest3 =
  nnfTTemplate "toNNF not affecting atom" simple1 simple1
nnfTest4 =
  nnfTTemplate "toNNF not affecting not atom" expr expr
  where expr = neg simple1
nnfTest5 =
  nnfTTemplate "toNNF not affecting or" expr expr
  where expr = simple1 \/ simple1
nnfTest6 =
  nnfTTemplate "toNNF not affecting and" expr expr
  where expr = simple1 /\ simple1
nnfTest7 =
  nnfTTemplate "toNNF simplifies not not" target expr
  where target = simple1
        expr   = neg (neg simple1)
nnfTest8 =
  nnfTTemplate "toNNF pushes not inside or" target expr
  where expr   = neg (simple1 \/ simple1)
        target = neg simple1 /\ neg simple1
nnfTest9 =
  nnfTTemplate "toNNF pushes not inside and" target expr
  where expr   = neg (simple1 /\ simple1)
        target = neg simple1 \/ neg simple1
nnfTest10 =
  nnfTTemplate "toNNF pushes inside exists" target expr
  where expr   = neg (forall "R" simple1)
        target = exists "R" (neg simple1) 
nnfTest11 =
  nnfTTemplate "toNNF pushes inside exists" target expr
  where expr   = neg (exists "R" top)
        target = forall "R" bottom

nnfTests = TestList $ map testLabel [("nnf test1", nnfTest1),
                                     ("nnf test2", nnfTest2),
                                     ("nnf test3", nnfTest3),
                                     ("nnf test4", nnfTest4),
                                     ("nnf test5", nnfTest5),
                                     ("nnf test6", nnfTest6),
                                     ("nnf test7", nnfTest7),
                                     ("nnf test8", nnfTest8),
                                     ("nnf test9", nnfTest9),
                                     ("nnf test10", nnfTest10),
                                     ("nnf test11", nnfTest11)]

-- binding tests
bindTest1 = 
  TestCase (assertBool "And stronger than or" $ part1 == part2)
  where part1, part2 :: Concept
        part1 = top \/ bottom /\ top
        part2 = top \/ (bottom /\ top)
bindTest2 =
   TestCase (assertBool "Neg stronger than anything" $ part1 == part2)
   where part1, part2 :: Concept
         part1 = neg top /\ bottom 
         part2 = neg (top) /\ bottom
bindTest3 =
   TestCase (assertBool "Or stronger than subset" $ part1 == part2)
   where part1, part2 :: Concept
         part1 = (atom "C") \/ (atom "D") .> top
         part2 = ((atom "C") \/ (atom "D")) .> top
bindTest4 =
   TestCase (assertBool "Anything stronger than equals" $ part1 == part2)
   where part1, part2 :: Concept
         part1 = top /\ bottom <-> bottom \/ top
         part2 = (top /\ bottom) <-> (bottom \/ top)

bindTests = TestList $ map testLabel [ ("bind test 1", bindTest1),
                                       ("bind test 2", bindTest2),
                                       ("bind test 3", bindTest3),
                                       ("bind test 4", bindTest4)]

-- testing the atomic function

atomicTest1 = TestCase (assertBool "Atomic simple 1" $ (isAtomic bottom) == True)
atomicTest2 = TestCase (assertBool "Atomic simple 2" $ (isAtomic simple1) == True)
atomicTest3 = TestCase (assertBool "Non atomic" $ (isAtomic simple6) == False)

atomicTests = TestList $ map testLabel [ ("atomic test 1", atomicTest1),
                                         ("atomic test 2", atomicTest2),
                                         ("atomic test 3", atomicTest3) ]
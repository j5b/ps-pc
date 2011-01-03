{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   Description: Testing console output
-}

module ConsoleOutput_test where

import TestUtils
import ConsoleOutput
import Model
import Proof
import Signature

import Test.HUnit

testmodeloutput1 = TestCase $ modelToConsole "temp" emptyModel

testmodeloutput2 = TestCase $ modelToConsole "temp" input
    where input  = ([1,2],[("A",[1]),("B",[2])],[("R",[(1,2),(2,1)])])

modeloutputtests = maplabel "Model output" [testmodeloutput1, testmodeloutput2]

testproofoutput1 = TestCase $ proofToConsole "temp" input
    where input  = (NodeZero ([bottom], "", bottom))

testproofoutput2 = TestCase $ proofToConsole "temp" input
    where input  = (NodeOne ([And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A"))),Exists "R" T],"and",And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A")))) (NodeOne ([Exists "R" T,Forall "R" (Atom "A"),Forall "R" (Neg (Atom "A"))],"exists",Exists "R" T) (NodeZero ([Atom "A",Neg (Atom "A"),And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A"))),Exists "R" T,T],"bottom",Atom "A"))))

proofoutputtests = maplabel "Proof output" [testproofoutput1, testproofoutput2]

testco1 = testequality msg target result $ show input
    where msg    = "Failed to show a very simple concept 1"
          target = "top&bot"
          result = conceptToConsole input 
          input  = top /\ bottom
testco2 = testequality msg target result $ show input
    where msg    = "Failed to show a very simple concept 2"
          target = "(A|B)&C"
          result = conceptToConsole input
          input  = (atom "A" \/ atom "B") /\ atom "C"
testco3 = testequality msg target result $ show input
    where msg    = "Failed to show a very simple concept 3"
          target = "Exists R (C)"
          result = conceptToConsole input
          input  = exists "R" $ atom "C"
testco4 = testequality msg target result $ show input
    where msg    = "Failed to show a very simple concept 4"
          target = "Forall R (Exists S (top))"
          result = conceptToConsole input
          input  = forall "R" $ exists "S" top
testco5 = testequality msg target result $ show input
    where msg    = "Failed to show a complex concept 1"
          target = "(Forall R (Exists S (top)))|(Forall S (Exists R (bot)))"
          result = conceptToConsole input
          input  = (forall "R" $ exists "S" top) \/ (forall "S" $ exists "R" bottom)
testco6 = testequality msg target result $ show input
    where msg    = "Failed to show a complex concept 2"
          target = "top&(Exists R (bot|(Forall S (A|B))))"
          result = conceptToConsole input
          input  = top /\ (exists "R" (bottom \/ forall "S" (atom "A" \/ atom "B")))

concepttoconsoletests = maplabel "Concept to console output" list
    where list   = [testco1, testco2, testco3, testco4, testco5, testco6]

allconsoleoutputtests = do putStrLn "Testing all relating function to console output"
                           runTestTT concepttoconsoletests
                           runTestTT modeloutputtests
                           runTestTT proofoutputtests
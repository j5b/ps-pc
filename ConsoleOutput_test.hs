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

testmodeloutput1 = testequality msg target result $ printModel emptyModel
    where msg    = "Failed to output correctly empty model"
          target = "Domain  = []\n"++
                   "No unary relation\n"++
                   "No binary relation\n"
          result = modelToConsole emptyModel
testmodeloutput2 = testequality msg target result $ printModel input
    where msg    = "Failed to output a simple model correctly"
          target = "Domain  = [1,2]\n"++
                   "Unarys  = \n"++
                   "--Unary A is satisfied for: [1]\n"++
                   "--Unary B is satisfied for: [2]\n"++
                   "Binarys = \n"++
                   "--Binary R is satisfied for: [(1,2),(2,1)]\n"
          result = modelToConsole input
          input  = ([1,2],[("A",[1]),("B",[2])],[("R",[(1,2),(2,1)])])

modeloutputtests = maplabel "Model output" [testmodeloutput1, testmodeloutput2]

testproofoutput1 = testequality msg target result $ show input
    where msg    = "Failed to output the simplest of proof"
          target = "bot\n"++
                   "=Contradiction on: bot\n"++
                   "--UNSAT--\n"
          result = proofToConsole input
          input  = (NodeZero ([bottom], "", bottom))
testproofoutput2 = testequality msg target result $ show input
    where msg    = "Failed to output a simple proof"
          target = "(Forall R (A))&(Forall R (~A)),Exists R (top)\n"++
                   "=By rule 'and' applied on concept (Forall R (A))&(Forall R (~A))\n"++
                   "Exists R (top),Forall R (A),Forall R (~A)\n"++
                   "=By rule 'exists' applied on concept Exists R (top)\n"++
                   "A,~A,(Forall R (A))&(Forall R (~A)),Exists R (top),top\n"++
                   "=Contradiction on: A\n"++
                   "--UNSAT--\n"
          result = proofToConsole input
          input  = (NodeOne ([And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A"))),Exists "R" T],"and",And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A")))) (NodeOne ([Exists "R" T,Forall "R" (Atom "A"),Forall "R" (Neg (Atom "A"))],"exists",Exists "R" T) (NodeZero ([Atom "A",Neg (Atom "A"),And (Forall "R" (Atom "A")) (Forall "R" (Neg (Atom "A"))),Exists "R" T,T],"bottom",Atom "A"))))

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
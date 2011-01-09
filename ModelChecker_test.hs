{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: tests the model checker
-}

module ModelChecker_test where 

import ModelChecker
import Model
import Signature
import TestUtils

import Test.HUnit
import Data.Maybe

allmodelcheckertests = do putStrLn "==== Testing the model checker for empty gamma"
                          runTestTT emptyTests
                          runTestTT test1List
                          runTestTT test2List
                          putStrLn "==== Testing the model checker for non-empty gamma"
                          runTestTT test3List

-- emptyCase

testEmpty1 = testequality msg result target "empty model"
  where msg    = "Failed to show C doesn't hold in empty model"
        result = checkGivens atomc emptyModel
        target = ("Empty model", False)
testEmpty2 = testequality msg result target "empty model"
  where msg    = "Failed to show T doesn't hold in empty model"
        result = checkGivens top emptyModel
        target = ("Empty model", False)
testEmpty3 = testequality msg result target "empty model"
  where msg    = "Failed to show not C doesn't hold in empty model"
        result = checkGivens (neg atomc) emptyModel
        target = ("Empty model", False)
testEmpty4 = testequality msg result target "empty model"
  where msg    = "Failed to show forall R C or T doesn't hold in empty model"
        result = checkGivens (forall_r (atomc \/ top)) emptyModel
        target = ("Empty model", False)
testEmpty5 = testequality msg result target "empty model"
  where msg    = "Failed to show exists R C and not T doesn't hold in empty model"
        result = checkGivens (exists_r (atomc /\ bottom)) emptyModel
        target = ("Empty model", False)

emptyTests = 
  maplabel "Empty model, model checker" [testEmpty1, testEmpty2, testEmpty3, testEmpty4, testEmpty5]

-- first simple model

model1 = (domain, unarys, binarys)
  where domain = [1] -- simple huh?
        unarys = [("C", [1]),
                  ("D", [1])] -- simple huh?
        binarys = [("R", []),
                   ("S", [])] -- simple huh?
test11 = testequality "Failed to show top is true" result target $ printModel model1
  where result = checkGivens top model1 
        target = ("", True)
test12 = testequality "Failed to show bottom is false" result target $ printModel model1
  where result = checkGivens bottom model1
        target = ("Failed to satisfy bottom for 1\n", False)
test13 = testequality "Failed to show C is true" result target $ printModel model1
  where result = checkGivens atomc model1
        target = ("", True)
test14 = testequality "Failed to show D is true" result target $ printModel model1
  where result = checkGivens atomd model1
        target = ("", True)
test15 = testequality "Failed to show not C is false" result target $ printModel model1
  where result = checkGivens notatomc model1
        target = ("Failed to not satisfy C for 1\n", False)
test16 = testequality "Failed to show not D is false" result target $ printModel model1
  where result = checkGivens notatomd model1
        target = ("Failed to not satisfy D for 1\n", False)
test17 = testequality "Failed to show C or D is true" result target $ printModel model1
  where result = checkGivens c_or_d model1
        target = ("", True)
test18 = testequality "Failed to show C and D is true" result target $ printModel model1
  where result = checkGivens c_and_d model1
        target = ("", True)
test19 = testequality "Failed to show C or not D is true" result target $ printModel model1
  where result = checkGivens c_or_nd model1
        target = ("", True)
test110 = testequality "Failed to show C and not D is false" result target $ printModel model1
  where result = checkGivens c_and_nd model1
        target = ("Failed to not satisfy D for 1\n", False)
test111 = testequality "Failed to show C and top is true" result target $ printModel model1
  where result = checkGivens c_and_top model1
        target = checkGivens atomc model1
test112 = testequality "Failed to show C and bottom is false" result target $ printModel model1
  where result = checkGivens c_and_bottom model1
        target = ("Failed to satisfy bottom for 1\n", False)
test113 = testequality "Failed to show C or top is false" result target $ printModel model1
  where result = checkGivens c_or_top model1
        target = ("", True)
test114 = testequality "Failed to show C or bottom is true" result target $ printModel model1
  where result = checkGivens c_or_bottom model1
        target = ("", True)
test115 = testequality "Failed to show forall R top is true" result target $ printModel model1
  where result = checkGivens forall_r_top model1
        target = ("", True)
test116 = testequality "Failed to show forall R bottom is true" result target $ printModel model1
  where result = checkGivens forall_r_bottom model1
        target = ("", True)
test117 = testequality "Failed to show forall R C is true" result target $ printModel model1
  where result = checkGivens forall_r_c model1
        target = ("", True)
test118 = testequality "Failed to show forall R D is true" result target $ printModel model1
  where result = checkGivens forall_r_d model1
        target = ("", True)
test119 = testequality "Failed to show forall R not C is true" result target $ printModel model1
  where result = checkGivens forall_r_nc model1
        target = ("", True)
test120 = testequality "Failed to show forall S C is true" result target $ printModel model1
  where result = checkGivens forall_s_c model1
        target = ("", True)
test121 = testequality "Failed to show forall S C and D is true" result target $ printModel model1
  where result = checkGivens (forall_s c_and_d) model1
        target = ("", True)
test122 = testequality "Failed to show forall S C or D is true" result target $ printModel model1
  where result = checkGivens (forall_s c_or_d) model1
        target = ("", True)
test123 = testequality "Failed to show forall S C and not T is true" result target $ printModel model1
  where result = checkGivens (forall_s c_and_bottom) model1
        target = ("", True)
test124 = testequality "Failed to show exists R T is false" result target $ printModel model1
  where result = checkGivens exists_r_top model1
        target = ("No successors for relation R for 1\n", False)
test125 = testequality "Failed to show exists R not T is false" result target $ printModel model1
  where result = checkGivens exists_r_bottom model1
        target = ("No successors for relation R for 1\n", False)
test126 = testequality "Failed to show exists R C is false" result target $ printModel model1
  where result = checkGivens exists_r_c model1
        target = ("No successors for relation R for 1\n", False)
test127 = testequality "Failed to show exists R D is false" result target $ printModel model1
  where result = checkGivens exists_r_d model1
        target = ("No successors for relation R for 1\n", False)
test128 = testequality "Failed to show exists S not D is false" result target $ printModel model1
  where result = checkGivens exists_s_nd model1
        target = ("No successors for relation S for 1\n", False)
test129 = testequality "Failed to show exists S C and D is false" result target $ printModel model1
  where result = checkGivens (exists_s c_and_d) model1
        target = ("No successors for relation S for 1\n", False)
test130 = testequality "Failed to show exists S C or D is false" result target $ printModel model1
  where result = checkGivens (exists_s c_or_d) model1
        target = ("No successors for relation S for 1\n", False)
test131 = testequality "Failed to show exists S C and not T is false" result target $ printModel model1
  where result = checkGivens (exists_s c_and_d) model1
        target = ("No successors for relation S for 1\n", False)

test1List = maplabel "Simple model, model checker testing" list
  where list = [test11, test12, test13, test14, test15,
                test16, test17, test18, test19, test110,
                test111, test112, test113, test114, test115,
                test116, test117, test118, test119, test120,
                test121, test122, test123, test124, test125,
                test126, test127, test128, test129, test130, test131]

-- Complicated model still empty knowledge base

model2 = (domain, unarys, binarys)
  where domain = [1,2,3,4,5] -- more :)
        unarys = [("C", [1,3,5]),
                  ("D", [2,4,5])] -- more :)
        binarys = [("R", [(1,2),(2,3),(3,4),(4,1)]),
                   ("S", [(5,1),(5,2),(5,3),(5,4)])] -- more
test21 = testequality "Failed to show top is true" result target $ printModel model2
  where result = checkGivens top model2 
        target = ("", True)
test22 = testequality "Failed to show bottom is false" result target $ printModel model2
  where result = checkGivens bottom model2
        target = ("Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy bottom for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy bottom for 4\n"++
                  "Failed to satisfy bottom for 5\n", False)
test23 = testequality "Failed to show C is true" result target $ printModel model2
  where result = checkGivens atomc model2
        target = ("", True)
test24 = testequality "Failed to show D is true" result target $ printModel model2
  where result = checkGivens atomd model2
        target = ("", True)
test25 = testequality "Failed to show not C is true" result target $ printModel model2
  where result = checkGivens notatomc model2
        target = ("", True)
test26 = testequality "Failed to show not D is true" result target $ printModel model2
  where result = checkGivens notatomd model2
        target = ("", True)
test27 = testequality "Failed to show C or D is true" result target $ printModel model2
  where result = checkGivens c_or_d model2
        target = ("", True)
test28 = testequality "Failed to show C and D is true" result target $ printModel model2
  where result = checkGivens c_and_d model2
        target = ("", True)
test29 = testequality "Failed to show C or not D is true" result target $ printModel model2
  where result = checkGivens c_or_nd model2
        target = ("", True)
test210 = testequality "Failed to show C and not D is true" result target $ printModel model2
  where result = checkGivens c_and_nd model2
        target = ("", True)
test211 = testequality "Failed to show C and top is true" result target $ printModel model2
  where result = checkGivens c_and_top model2
        target = checkGivens atomc model2
test212 = testequality "Failed to show C and bottom is false" result target $ printModel model2
  where result = checkGivens c_and_bottom model2
        target = ("Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy C for 4\n"++
                  "Failed to satisfy bottom for 5\n", False)
test213 = testequality "Failed to show C or top is true" result target $ printModel model2
  where result = checkGivens c_or_top model2
        target = ("", True)
test214 = testequality "Failed to show C or bottom is true" result target $ printModel model2
  where result = checkGivens c_or_bottom model2
        target = ("", True)
test215 = testequality "Failed to show forall R top is true" result target $ printModel model2
  where result = checkGivens forall_r_top model2
        target = ("", True)
test216 = testequality "Failed to show forall R bottom is true" result target $ printModel model2
  where result = checkGivens forall_r_bottom model2
        target = ("", True)
test217 = testequality "Failed to show forall R C is true" result target $ printModel model2
  where result = checkGivens forall_r_c model2
        target = ("", True)
test218 = testequality "Failed to show forall R D is true" result target $ printModel model2
  where result = checkGivens forall_r_d model2
        target = ("", True)
test219 = testequality "Failed to show forall R not C is false" result target $ printModel model2
  where result = checkGivens forall_r_nc model2
        target = ("", True)
test220 = testequality "Failed to show forall S C is true" result target $ printModel model2
  where result = checkGivens forall_s_c model2
        target = ("", True)
test221 = testequality "Failed to show forall S C and D is true" result target $ printModel model2
  where result = checkGivens (forall_s c_and_d) model2
        target = ("", True)
test222 = testequality "Failed to show forall S C or D is true" result target $ printModel model2
  where result = checkGivens (forall_s c_or_d) model2
        target = ("", True)
test223 = testequality "Failed to show forall S C and not T is true" result target $ printModel model2
  where result = checkGivens (forall_s c_and_bottom) model2
        target = ("", True)
test224 = testequality "Failed to show exists R T is true" result target $ printModel model2
  where result = checkGivens exists_r_top model2
        target = ("", True)
test225 = testequality "Failed to show exists R not T is false" result target $ printModel model2
  where result = checkGivens exists_r_bottom model2
        target = ("Failed to satisfy bottom for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy bottom for 4\n"++
                  "Failed to satisfy bottom for 1\n"++
                  "No successors for relation R for 5\n", False)
test226 = testequality "Failed to show exists R C is true" result target $ printModel model2
  where result = checkGivens exists_r_c model2
        target = ("", True)
test227 = testequality "Failed to show exists R D is true" result target $ printModel model2
  where result = checkGivens exists_r_d model2
        target = ("", True)
test228 = testequality "Failed to show exists S not D is true" result target $ printModel model2
  where result = checkGivens exists_s_nd model2
        target = ("", True)
test229 = testequality "Failed to show exists S C and D is false" result target $ printModel model2
  where result = checkGivens (exists_s c_and_d) model2
        target = ("No successors for relation S for 1\n"++
                  "No successors for relation S for 2\n"++
                  "No successors for relation S for 3\n"++
                  "No successors for relation S for 4\n"++
                  "Failed to satisfy D for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy D for 3\n"++
                  "Failed to satisfy C for 4\n", False)
test230 = testequality "Failed to show exists S C or D is true" result target $ printModel model2
  where result = checkGivens (exists_s c_or_d) model2
        target = ("", True)
test231 = testequality "Failed to show exists S C and not T is false" result target $ printModel model2
  where result = checkGivens (exists_s c_and_bottom) model2
        target = ("No successors for relation S for 1\n"++
                  "No successors for relation S for 2\n"++
                  "No successors for relation S for 3\n"++
                  "No successors for relation S for 4\n"++
                  "Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy C for 4\n", False)

test2List = maplabel "Complex model, Model Checker testing" list
  where list = [test21, test22, test23, test24, test25, test26, test27,
                test28, test29, test210, test211, test212, test213, test214,
                test215, test216, test217, test218, test219, test220, test221,
                test222, test223, test224, test225, test226, test227, test228,
                test229, test230, test231]

-- Model for Gamma not empty

gammaModel = (domain, unarys, binarys)
  where domain  = [1,2,3,4,5,6]
        unarys  = [("C", [1,2,3,4,5,6]),
                  ("D", [2,4,6])]
        binarys = [("R", [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)]),
                   ("S", [(2,4),(4,6),(6,2)])]

test31 = testequality "Failed to show top is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [top] [] 
        target = True
test32 = testequality "Failed to show bottom is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [bottom] [] 
        target = False
test33 = testequality "Failed to show C is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [atomc] [] 
        target = True
test34 = testequality "Failed to show D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [atomd] [] 
        target = False
test35 = testequality "Failed to show not C is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [notatomc] [] 
        target = False
test36 = testequality "Failed to show not D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [notatomd] [] 
        target = False
test37 = testequality "Failed to show C or D is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_or_d] [] 
        target = True
test38 = testequality "Failed to show C and D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_and_d] [] 
        target = False
test39 = testequality "Failed to show C or not D is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_or_nd] [] 
        target = True
test310 = testequality "Failed to show C and not D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_and_nd] [] 
        target = False
test311 = testequality "Failed to show C and top is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_and_top] [] 
        target = True
test312 = testequality "Failed to show C and bottom is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_and_bottom] [] 
        target = False
test313 = testequality "Failed to show C or top is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_or_top] [] 
        target = True
test314 = testequality "Failed to show C or bottom is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [c_or_bottom] [] 
        target = True
test315 = testequality "Failed to show forall R top is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_r_top] [] 
        target = True
test316 = testequality "Failed to show forall R bottom is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_r_bottom] [] 
        target = False
test317 = testequality "Failed to show forall R C is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_r_c] [] 
        target = True
test318 = testequality "Failed to show forall R D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_r_d] [] 
        target = False
test319 = testequality "Failed to show forall R not C is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_r_nc] [] 
        target = False
test320 = testequality "Failed to show forall S C is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_s_c] [] 
        target = True
test321 = testequality "Failed to show forall S C and D is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_s c_and_d] [] 
        target = True
test322 = testequality "Failed to show forall S C or D is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_s c_or_d] [] 
        target = True
test323 = testequality "Failed to show forall S C and not T is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [forall_s c_and_bottom] [] 
        target = False
test324 = testequality "Failed to show exists R T is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_r_top] [] 
        target = True
test325 = testequality "Failed to show exists R not T is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_r_bottom] [] 
        target = False
test326 = testequality "Failed to show exists R C is true" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_r_c] [] 
        target = True
test327 = testequality "Failed to show exists R D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_r_d] [] 
        target = False
test328 = testequality "Failed to show exists S not D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_s_nd] [] 
        target = False
test329 = testequality "Failed to show exists S C and D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_s c_and_d] [] 
        target = False
test330 = testequality "Failed to show exists S C or D is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_s c_or_d] [] 
        target = False
test331 = testequality "Failed to show exists S C and not T is false" result target $ printModel gammaModel
  where result = snd $ checkInputModel gammaModel [exists_s c_and_d] [] 
        target = False

test3List = maplabel "Non empty gamma, Model Checker testing" list
  where list = [test31, test32, test33, test34, test35, test36, test37,
                test38, test39, test310, test311, test312, test313, test314,
                test315, test316, test317, test318, test319, test320, test321,
                test322, test323, test324, test325, test326, test327, test328,
                test329, test330, test331]

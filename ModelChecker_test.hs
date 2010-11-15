{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: tests the model checker
-}

import Test.HUnit
import Data.Maybe
import ModelChecker
import Model
import Signature

label (a,b) = TestLabel a b
template msg result target =
  TestCase (assertEqual msg target result)

-- emptyCase

testEmpty1 = template msg result target
  where msg    = "Chceking if C doesn't hold in empty model"
        result = checkModel (atom "C") emptyModel
        target = ("Empty model", False)
testEmpty2 = template msg result target
  where msg    = "Chceking if T doesn't hold in empty model"
        result = checkModel top emptyModel
        target = ("Empty model", False)
testEmpty3 = template msg result target
  where msg    = "Chceking if not C doesn't hold in empty model"
        result = checkModel (neg $ atom "C") emptyModel
        target = ("Empty model", False)
testEmpty4 = template msg result target
  where msg    = "Chceking if forall R C or T doesn't hold in empty model"
        result = checkModel (forall "R" (atom "C" \/ top)) emptyModel
        target = ("Empty model", False)
testEmpty5 = template msg result target
  where msg    = "Chceking if exists D X and not T doesn't hold in empty model"
        result = checkModel (exists "D" (atom "X" /\ bottom)) emptyModel
        target = ("Empty model", False)

emptyTests = TestList $ map label [("", testEmpty1),
                                   ("", testEmpty2),
                                   ("", testEmpty3),
                                   ("", testEmpty4),
                                   ("", testEmpty5)]


-- simple concepts to test
sc1 = top -- always true
sc2 = bottom -- always false
sc3 = atom "C" 
sc4 = atom "D"
sc5 = neg sc3
sc6 = neg sc4
sc7 = sc3 \/ sc4
sc8 = sc3 /\ sc4
sc9 = sc3 \/ (neg sc4)
sc10 = sc3 /\ (neg sc4)
sc11 = sc3 /\ top -- should be the same as concept3
sc12 = sc3 /\ bottom -- alawys false
sc13 = sc3 \/ top -- always true
sc14 = sc3 \/ bottom -- should be the same as sc3
sc15 = forall "R" top -- should always be true
sc16 = forall "R" bottom -- should only be true if there are no successors
sc17 = forall "R" sc3
sc18 = forall "R" sc4
sc19 = forall "R" (neg sc3)
sc20 = forall "S" sc3
sc21 = forall "S" (sc3 /\ sc4)
sc22 = forall "S" (sc3 \/ sc4)
sc23 = forall "S" (sc3 /\ bottom) -- should be only true if there are no successors
sc24 = exists "R" top -- should always be true unless there are no successors
sc25 = exists "R" bottom -- should always be false
sc26 = exists "R" sc3
sc27 = exists "R" sc4
sc28 = exists "S" (neg sc4)
sc29 = exists "S" (sc3 /\ sc4)
sc30 = exists "S" (sc3 \/ sc4)
sc31 = exists "S" (sc3 /\ bottom) -- should be always false

-- first simple model

model1 = (domain, unarys, binarys)
  where domain = [1] -- simple huh?
        unarys = [("C", [1]),
                  ("D", [1])] -- simple huh?
        binarys = [("R", []),
                   ("S", [])] -- simple huh?
test11 = template "top is true" result target
  where result = checkModel sc1 model1 
        target = ("", True)
test12 = template "bottom is false" result target
  where result = checkModel sc2 model1
        target = ("Failed since we have to satisfy bottom for 1\n", False)
test13 = template "C is true" result target
  where result = checkModel sc3 model1
        target = ("", True)
test14 = template "D is true" result target
  where result = checkModel sc4 model1
        target = ("", True)
test15 = template "not C is false" result target
  where result = checkModel sc5 model1
        target = ("Failed not to satisfy C for 1\n", False)
test16 = template "not D is false" result target
  where result = checkModel sc6 model1
        target = ("Failed not to satisfy D for 1\n", False)
test17 = template "C or D is true" result target
  where result = checkModel sc7 model1
        target = ("", True)
test18 = template "C and D is true" result target
  where result = checkModel sc8 model1
        target = ("", True)
test19 = template "C or not D is true" result target
  where result = checkModel sc9 model1
        target = ("", True)
test110 = template "C and not D is false" result target
  where result = checkModel sc10 model1
        target = ("Failed not to satisfy D for 1\n", False)
test111 = template "C and top is true" result target
  where result = checkModel sc11 model1
        target = checkModel sc3 model1
test112 = template "C and bottom is false" result target
  where result = checkModel sc12 model1
        target = ("Failed since we have to satisfy bottom for 1\n", False)
test113 = template "C or top is false" result target
  where result = checkModel sc13 model1
        target = ("", True)
test114 = template "C or bottom is true" result target
  where result = checkModel sc14 model1
        target = ("", True)
test115 = template "forall R top is true" result target
  where result = checkModel sc15 model1
        target = ("", True)
test116 = template "forall R bottom is true" result target
  where result = checkModel sc16 model1
        target = ("", True)
test117 = template "forall R C is true" result target
  where result = checkModel sc17 model1
        target = ("", True)
test118 = template "forall R D is true" result target
  where result = checkModel sc18 model1
        target = ("", True)
test119 = template "forall R not C is true" result target
  where result = checkModel sc19 model1
        target = ("", True)
test120 = template "forall S C is true" result target
  where result = checkModel sc20 model1
        target = ("", True)
test121 = template "forall S C and D is true" result target
  where result = checkModel sc21 model1
        target = ("", True)
test122 = template "forall S C or D is true" result target
  where result = checkModel sc22 model1
        target = ("", True)
test123 = template "forall S C and not T is true" result target
  where result = checkModel sc23 model1
        target = ("", True)
test124 = template "exists R T is false" result target
  where result = checkModel sc24 model1
        target = ("No successors for relation R for 1\n", False)
test125 = template "exists R not T is false" result target
  where result = checkModel sc25 model1
        target = ("No successors for relation R for 1\n", False)
test126 = template "exists R C is false" result target
  where result = checkModel sc26 model1
        target = ("No successors for relation R for 1\n", False)
test127 = template "exists R D is false" result target
  where result = checkModel sc27 model1
        target = ("No successors for relation R for 1\n", False)
test128 = template "exists S not D is false" result target
  where result = checkModel sc28 model1
        target = ("No successors for relation S for 1\n", False)
test129 = template "exists S C and D is false" result target
  where result = checkModel sc29 model1
        target = ("No successors for relation S for 1\n", False)
test130 = template "exists S C or D is false" result target
  where result = checkModel sc30 model1
        target = ("No successors for relation S for 1\n", False)
test131 = template "exists S C and not T is false" result target
  where result = checkModel sc31 model1
        target = ("No successors for relation S for 1\n", False)

test1List = TestList $ map label [("", test11),
                                  ("", test12),
                                  ("", test13),
                                  ("", test14),
                                  ("", test15),
                                  ("", test16),
                                  ("", test17),
                                  ("", test18),
                                  ("", test19),
                                  ("", test110),
                                  ("", test111),
                                  ("", test112),
                                  ("", test113),
                                  ("", test114),
                                  ("", test115),
                                  ("", test116),
                                  ("", test117),
                                  ("", test118),
                                  ("", test119),
                                  ("", test120),
                                  ("", test121),
                                  ("", test122),
                                  ("", test123),
                                  ("", test124),
                                  ("", test125),
                                  ("", test126),
                                  ("", test127),
                                  ("", test128),
                                  ("", test129),
                                  ("", test130),
                                  ("", test131)]






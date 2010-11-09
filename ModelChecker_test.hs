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
concept1 = top -- always true
concept2 = bottom -- always false
concept3 = atom "C" 
concept4 = atom "D"
concept5 = neg concept3
concept6 = neg concept4
concept7 = concept3 \/ concept4
concept8 = concept3 /\ concept4
concept9 = concept3 \/ (neg concept4)
concept10 = concept3 /\ (neg concept4)
concept11 = concept3 /\ top -- should be the same as concept3
concept12 = concept3 /\ bottom -- alawys false
concept13 = concept3 \/ top -- always true
concept14 = concept3 \/ bottom -- should be the same as concept3
concept15 = forall "R" top -- should always be true
concept16 = forall "R" bottom -- should only be true if there are no successors
concept17 = forall "R" concept3
concept18 = forall "R" concept4
concept19 = forall "R" (neg concept3)
concept20 = forall "S" concept3
concept21 = forall "S" (concept3 /\ concept4)
concept22 = forall "S" (concept3 \/ concept4)
concept23 = forall "S" (concept3 /\ bottom) -- should be only true if there are no successors
concept24 = exists "R" top -- should always be true unless there are no successors
concept25 = exists "R" bottom -- should always be false
concept26 = exists "R" concept3
concept27 = exists "R" concept4
concept28 = exists "S" (neg concept4)
concept29 = exists "S" (concept3 /\ concept4)
concept30 = exists "S" (concept3 \/ concept4)
concept31 = exists "S" (concept3 /\ bottom) -- should be always false

-- first simple model

model1 = (domain, unarys, binarys)
  where domain = [1] -- simple huh?
        unarys = [("C", [1]),
                  ("D", [1])] -- simple huh?
        binarys = [("R", []),
                   ("S", [])] -- simple huh?
test11 = template "top is true" result target
  where result = checkModel concept1 model1 
        target = ("", True)
test12 = template "bottom is false" result target
  where result = checkModel concept2 model1
        target = ("Failed since we have to satisfy bottom for 1\n", False)
test13 = template "C is true" result target
  where result = checkModel concept3 model1
        target = ("", True)
test14 = template "D is true" result target
  where result = checkModel concept4 model1
        target = ("", True)
test15 = template "not C is false" result target
  where result = checkModel concept5 model1
        target = ("Failed not to satisfy C for 1\n", False)
test16 = template "not D is false" result target
  where result = checkModel concept6 model1
        target = ("Failed not to satisfy D for 1\n", False)
test17 = template "C or D is true" result target
  where result = checkModel concept7 model1
        target = ("", True)
test18 = template "C and D is true" result target
  where result = checkModel concept8 model1
        target = ("", True)
test19 = template "C or not D is true" result target
  where result = checkModel concept9 model1
        target = ("", True)
test110 = template "C and not D is false" result target
  where result = checkModel concept10 model1
        target = ("Failed not to satisfy D for 1\n", False)
test111 = template "C and top is true" result target
  where result = checkModel concept11 model1
        target = checkModel concept3 model1
test112 = template "C and bottom is false" result target
  where result = checkModel concept12 model1
        target = ("Failed since we have to satisfy bottom for 1\n", False)
test113 = template "C or top is false" result target
  where result = checkModel concept13 model1
        target = ("", True)
test114 = template "C or bottom is true" result target
  where result = checkModel concept14 model1
        target = ("", True)
test115 = template "forall R top is true" result target
  where result = checkModel concept15 model1
        target = ("", True)
test116 = template "forall R bottom is true" result target
  where result = checkModel concept16 model1
        target = ("", True)
test117 = template "forall R C is true" result target
  where result = checkModel concept17 model1
        target = ("", True)
test118 = template "forall R D is true" result target
  where result = checkModel concept18 model1
        target = ("", True)
test119 = template "forall R not C is true" result target
  where result = checkModel concept19 model1
        target = ("", True)
test120 = template "forall S C is true" result target
  where result = checkModel concept20 model1
        target = ("", True)
test121 = template "forall S C and D is true" result target
  where result = checkModel concept21 model1
        target = ("", True)
test122 = template "forall S C or D is true" result target
  where result = checkModel concept22 model1
        target = ("", True)
test123 = template "forall S C and not T is true" result target
  where result = checkModel concept23 model1
        target = ("", True)
test124 = template "exists R T is false" result target
  where result = checkModel concept24 model1
        target = ("No successors for relation R for 1\n", False)
test125 = template "exists R not T is false" result target
  where result = checkModel concept25 model1
        target = ("No successors for relation R for 1\n", False)
test126 = template "exists R C is false" result target
  where result = checkModel concept26 model1
        target = ("No successors for relation R for 1\n", False)
test127 = template "exists R D is false" result target
  where result = checkModel concept27 model1
        target = ("No successors for relation R for 1\n", False)
test128 = template "exists S not D is false" result target
  where result = checkModel concept28 model1
        target = ("No successors for relation S for 1\n", False)
test129 = template "exists S C and D is false" result target
  where result = checkModel concept29 model1
        target = ("No successors for relation S for 1\n", False)
test130 = template "exists S C or D is false" result target
  where result = checkModel concept30 model1
        target = ("No successors for relation S for 1\n", False)
test131 = template "exists S C and not T is false" result target
  where result = checkModel concept31 model1
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






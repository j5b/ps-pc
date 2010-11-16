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
import TestUtils

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
        target = ("Failed to satisfy bottom for 1\n", False)
test13 = template "C is true" result target
  where result = checkModel sc3 model1
        target = ("", True)
test14 = template "D is true" result target
  where result = checkModel sc4 model1
        target = ("", True)
test15 = template "not C is false" result target
  where result = checkModel sc5 model1
        target = ("Failed to not satisfy C for 1\n", False)
test16 = template "not D is false" result target
  where result = checkModel sc6 model1
        target = ("Failed to not satisfy D for 1\n", False)
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
        target = ("Failed to not satisfy D for 1\n", False)
test111 = template "C and top is true" result target
  where result = checkModel sc11 model1
        target = checkModel sc3 model1
test112 = template "C and bottom is false" result target
  where result = checkModel sc12 model1
        target = ("Failed to satisfy bottom for 1\n", False)
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

-- Complicated model still empty knowledge base

model2 = (domain, unarys, binarys)
  where domain = [1,2,3,4,5] -- more :)
        unarys = [("C", [1,3,5]),
                  ("D", [2,4,5])] -- more :)
        binarys = [("R", [(1,2),(2,3),(3,4),(4,1)]),
                   ("S", [(5,1),(5,2),(5,3),(5,4)])] -- more
test21 = template "top is true" result target
  where result = checkModel sc1 model2 
        target = ("", True)
test22 = template "bottom is false" result target
  where result = checkModel sc2 model2
        target = ("Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy bottom for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy bottom for 4\n"++
                  "Failed to satisfy bottom for 5\n", False)
test23 = template "C is true" result target
  where result = checkModel sc3 model2
        target = ("", True)
test24 = template "D is true" result target
  where result = checkModel sc4 model2
        target = ("", True)
test25 = template "not C is true" result target
  where result = checkModel sc5 model2
        target = ("", True)
test26 = template "not D is true" result target
  where result = checkModel sc6 model2
        target = ("", True)
test27 = template "C or D is true" result target
  where result = checkModel sc7 model2
        target = ("", True)
test28 = template "C and D is true" result target
  where result = checkModel sc8 model2
        target = ("", True)
test29 = template "C or not D is true" result target
  where result = checkModel sc9 model2
        target = ("", True)
test210 = template "C and not D is true" result target
  where result = checkModel sc10 model2
        target = ("", True)
test211 = template "C and top is true" result target
  where result = checkModel sc11 model2
        target = checkModel sc3 model2
test212 = template "C and bottom is false" result target
  where result = checkModel sc12 model2
        target = ("Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy C for 4\n"++
                  "Failed to satisfy bottom for 5\n", False)
test213 = template "C or top is true" result target
  where result = checkModel sc13 model2
        target = ("", True)
test214 = template "C or bottom is true" result target
  where result = checkModel sc14 model2
        target = ("", True)
test215 = template "forall R top is true" result target
  where result = checkModel sc15 model2
        target = ("", True)
test216 = template "forall R bottom is true" result target
  where result = checkModel sc16 model2
        target = ("", True)
test217 = template "forall R C is true" result target
  where result = checkModel sc17 model2
        target = ("", True)
test218 = template "forall R D is true" result target
  where result = checkModel sc18 model2
        target = ("", True)
test219 = template "forall R not C is false" result target
  where result = checkModel sc19 model2
        target = ("", True)
test220 = template "forall S C is true" result target
  where result = checkModel sc20 model2
        target = ("", True)
test221 = template "forall S C and D is true" result target
  where result = checkModel sc21 model2
        target = ("", True)
test222 = template "forall S C or D is true" result target
  where result = checkModel sc22 model2
        target = ("", True)
test223 = template "forall S C and not T is true" result target
  where result = checkModel sc23 model2
        target = ("", True)
test224 = template "exists R T is true" result target
  where result = checkModel sc24 model2
        target = ("", True)
test225 = template "exists R not T is false" result target
  where result = checkModel sc25 model2
        target = ("Failed to satisfy bottom for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy bottom for 4\n"++
                  "Failed to satisfy bottom for 1\n"++
                  "No successors for relation R for 5\n", False)
test226 = template "exists R C is true" result target
  where result = checkModel sc26 model2
        target = ("", True)
test227 = template "exists R D is true" result target
  where result = checkModel sc27 model2
        target = ("", True)
test228 = template "exists S not D is true" result target
  where result = checkModel sc28 model2
        target = ("", True)
test229 = template "exists S C and D is false" result target
  where result = checkModel sc29 model2
        target = ("No successors for relation S for 1\n"++
                  "No successors for relation S for 2\n"++
                  "No successors for relation S for 3\n"++
                  "No successors for relation S for 4\n"++
                  "Failed to satisfy D for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy D for 3\n"++
                  "Failed to satisfy C for 4\n", False)
test230 = template "exists S C or D is true" result target
  where result = checkModel sc30 model2
        target = ("", True)
test231 = template "exists S C and not T is false" result target
  where result = checkModel sc31 model2
        target = ("No successors for relation S for 1\n"++
                  "No successors for relation S for 2\n"++
                  "No successors for relation S for 3\n"++
                  "No successors for relation S for 4\n"++
                  "Failed to satisfy bottom for 1\n"++
                  "Failed to satisfy C for 2\n"++
                  "Failed to satisfy bottom for 3\n"++
                  "Failed to satisfy C for 4\n", False)

test2List = TestList $ map label [("", test21),
                                  ("", test22),
                                  ("", test23),
                                  ("", test24),
                                  ("", test25),
                                  ("", test26),
                                  ("", test27),
                                  ("", test28),
                                  ("", test29),
                                  ("", test210),
                                  ("", test211),
                                  ("", test212),
                                  ("", test213),
                                  ("", test214),
                                  ("", test215),
                                  ("", test216),
                                  ("", test217),
                                  ("", test218),
                                  ("", test219),
                                  ("", test220),
                                  ("", test221),
                                  ("", test222),
                                  ("", test223),
                                  ("", test224),
                                  ("", test225),
                                  ("", test226),
                                  ("", test227),
                                  ("", test228),
                                  ("", test229),
                                  ("", test230),
                                  ("", test231)]


-- Model for Gamma not empty

gammaModel = (domain, unarys, binarys)
  where domain  = [1,2,3,4,5,6]
        unarys  = [("C", [1,2,3,4,5,6]),
                  ("D", [2,4,6])]
        binarys = [("R", [(1,2),(2,3),(3,4),(4,5),(5,6),(6,1)]),
                   ("S", [(2,4),(4,6),(6,2)])]

test31 = template "top is true" result target
  where result = snd $ checkInputModel gammaModel [sc1] [] 
        target = True
test32 = template "bottom is false" result target
  where result = snd $ checkInputModel gammaModel [sc2] [] 
        target = False
test33 = template "C is true" result target
  where result = snd $ checkInputModel gammaModel [sc3] [] 
        target = True
test34 = template "D is false" result target
  where result = snd $ checkInputModel gammaModel [sc4] [] 
        target = False
test35 = template "not C is false" result target
  where result = snd $ checkInputModel gammaModel [sc5] [] 
        target = False
test36 = template "not D is false" result target
  where result = snd $ checkInputModel gammaModel [sc6] [] 
        target = False
test37 = template "C or D is true" result target
  where result = snd $ checkInputModel gammaModel [sc7] [] 
        target = True
test38 = template "C and D is false" result target
  where result = snd $ checkInputModel gammaModel [sc8] [] 
        target = False
test39 = template "C or not D is true" result target
  where result = snd $ checkInputModel gammaModel [sc9] [] 
        target = True
test310 = template "C and not D is false" result target
  where result = snd $ checkInputModel gammaModel [sc10] [] 
        target = False
test311 = template "C and top is true" result target
  where result = snd $ checkInputModel gammaModel [sc11] [] 
        target = True
test312 = template "C and bottom is false" result target
  where result = snd $ checkInputModel gammaModel [sc12] [] 
        target = False
test313 = template "C or top is true" result target
  where result = snd $ checkInputModel gammaModel [sc13] [] 
        target = True
test314 = template "C or bottom is true" result target
  where result = snd $ checkInputModel gammaModel [sc14] [] 
        target = True
test315 = template "forall R top is true" result target
  where result = snd $ checkInputModel gammaModel [sc15] [] 
        target = True
test316 = template "forall R bottom is false" result target
  where result = snd $ checkInputModel gammaModel [sc16] [] 
        target = False
test317 = template "forall R C is true" result target
  where result = snd $ checkInputModel gammaModel [sc17] [] 
        target = True
test318 = template "forall R D is false" result target
  where result = snd $ checkInputModel gammaModel [sc18] [] 
        target = False
test319 = template "forall R not C is false" result target
  where result = snd $ checkInputModel gammaModel [sc19] [] 
        target = False
test320 = template "forall S C is true" result target
  where result = snd $ checkInputModel gammaModel [sc20] [] 
        target = True
test321 = template "forall S C and D is true" result target
  where result = snd $ checkInputModel gammaModel [sc21] [] 
        target = True
test322 = template "forall S C or D is true" result target
  where result = snd $ checkInputModel gammaModel [sc22] [] 
        target = True
test323 = template "forall S C and not T is false" result target
  where result = snd $ checkInputModel gammaModel [sc23] [] 
        target = False
test324 = template "exists R T is true" result target
  where result = snd $ checkInputModel gammaModel [sc24] [] 
        target = True
test325 = template "exists R not T is false" result target
  where result = snd $ checkInputModel gammaModel [sc25] [] 
        target = False
test326 = template "exists R C is true" result target
  where result = snd $ checkInputModel gammaModel [sc26] [] 
        target = True
test327 = template "exists R D is false" result target
  where result = snd $ checkInputModel gammaModel [sc27] [] 
        target = False
test328 = template "exists S not D is false" result target
  where result = snd $ checkInputModel gammaModel [sc28] [] 
        target = False
test329 = template "exists S C and D is false" result target
  where result = snd $ checkInputModel gammaModel [sc29] [] 
        target = False
test330 = template "exists S C or D is false" result target
  where result = snd $ checkInputModel gammaModel [sc30] [] 
        target = False
test331 = template "exists S C and not T is false" result target
  where result = snd $ checkInputModel gammaModel [sc31] [] 
        target = False

test3List = TestList $ map label [("", test31),
                                  ("", test32),
                                  ("", test33),
                                  ("", test34),
                                  ("", test35),
                                  ("", test36),
                                  ("", test37),
                                  ("", test38),
                                  ("", test39),
                                  ("", test310),
                                  ("", test311),
                                  ("", test312),
                                  ("", test313),
                                  ("", test314),
                                  ("", test315),
                                  ("", test316),
                                  ("", test317),
                                  ("", test318),
                                  ("", test319),
                                  ("", test320),
                                  ("", test321),
                                  ("", test322),
                                  ("", test323),
                                  ("", test324),
                                  ("", test325),
                                  ("", test326),
                                  ("", test327),
                                  ("", test328),
                                  ("", test329),
                                  ("", test330),
                                  ("", test331)]

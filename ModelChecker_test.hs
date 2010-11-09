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
  TestCase (assertEqual msg result target)

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
        result = checkModel (neg atom "C") emptyModel
        target = ("Empty model", False)
testEmpty4 = template msg result target
  where msg    = "Chceking if forall R C or T doesn't hold in empty model"
        result = checkModel (forall "R" (atom "C" \/ top)) emptyModel
        target = ("Empty model", False)
testEmpty5 = template msg result target
  where msg    = "Chceking if exists D X and not T doesn't hold in empty model"
        result = checkModel (exists "D" (atom "X" /\ bottom)) emptyModel
        target = ("Empty model", False)

-- simple concepts to test
concept1 = top
concept2 = bottom
concept3 = atom "C"
concept4 = atom "D"
concept5 = neg concept3
concept6 = neg concept4
concept7 = concept3 \/ concept4
concept8 = concept3 /\ concept4
concept9 = concept3 \/ (neg concept4)
concept10 = concept3 /\ (neg concept4)
concept11 = concept3 /\ top
concept12 = concept3 /\ bottom
concept13 = concept3 \/ top
concept14 = concept3 \/ bottom
concept15 = forall "R" top
concept16 = forall "R" bottom
concept17 = forall "R" concept3
concept18 = forall "R" concept4
concept19 = forall "R" (neg concept3)
concept20 = forall "S" concept3
concept21 = forall "S" (concept3 /\ concept4)
concept22 = forall "S" (concept3 \/ concept4)
concept23 = forall "S" (concept3 /\ bottom)
concept24 = exists "R" top
concept25 = exists "R" bottom
concept26 = exists "R" concept3
concept27 = exists "R" concept4
concept28 = exists "S" (neg concept4)
concept29 = exists "S" (concept3 /\ concept4)
concept30 = exists "S" (concept3 \/ concept4)
concept31 = exists "S" (concept3 /\ bottom)

-- first simple model

simpleModel1 = (domain, unarys, binarys)
{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model_test.hs
   Description: test models
-}

module Model_test where 

import Model
import TestUtils

import Test.HUnit

testmodelsorter1 = testequality msg target emptyModel $ printModel emptyModel
  where target = sortModel emptyModel
        msg    = "Failed to sort the empty model"

testmodelsorter2 = testequality msg target result $ printModel input
  where input  = ([1,2,3],[("A",[1,2]),("C",[2,3])],[("R",[(1,2),(2,3),(3,1)])])
        target = input
        result = sortModel input
        msg    = "Failed while sorting an already sorted model"

testmodelsorter3 = testequality msg target result $ printModel input
  where input  = ([1,3,2,4],[("B",[1,2]),("A",[3,2,4])],[("S",[(1,1)]),("R",[(2,1),(1,3)])]) 
        target = ([1,2,3,4],[("A",[2,3,4]),("B",[1,2])],[("R",[(1,3),(2,1)]),("S",[(1,1)])])
        result = sortModel input
        msg    = "Failed to sort a simple unsorted model"

modelsortertests = maplabel "Model sorter" [testmodelsorter1, testmodelsorter2, testmodelsorter3]

allmodelsortertests = do putStrLn "==== Testing Model sorter"
                         runTestTT modelsortertests
                        
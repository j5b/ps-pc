{-
  Author: Michal Parusinski
  Maintainer: Michal Parusinski
  Email: <mparusinski@googlemail.com>
  License: GPL 3.0
-}

module Reader_test where

import Reader
import TestUtils
import Proof
import ProofSearch
import Signature

import Test.HUnit

test11 = TestCase $ outputResult None (Left ([1],[],[])) ""
test12 = TestCase $ outputResult None (Right (NodeZero ([Neg T],"",Neg T))) ""

test21 = TestCase $ executeCommand Help
test22 = TestCase $ executeCommand (Solve None ([T], [T]) "output")

readertests = maplabel "Reader test" list
    where list = [test11,test12,test21,test22]

allreadertests = do putStrLn "Running tests for command reader for main"
                    runTestTT readertests
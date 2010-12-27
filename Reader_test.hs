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

test11 = outputResult None (Left ([1],[],[]))
test12 = outputResult None (Right (NodeZero ([Neg T],"",Neg T)))

test21 = executeCommand Help
test22 = executeCommand (Solve None ([T], [T]))

test31 = processString "console \"(top)\" \"(top)\""
test32 = processString "console \"(bottom)\" \"(bottom)\""

readertests = maplabel "Reader test" list
    where list = [test11,test12,test21,test22,test31,test32]

allreadertests = do putStrLn "Running tests for command reader for main"
                    runTestTT readertests
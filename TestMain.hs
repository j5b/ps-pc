{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: mparusinski@googlemail.com	
   License: GPL 3.0
   File: TestMain.hs
   Description: Perform all the tests
-}

module Main (main) where

-- import Proof_test
import ProofChecker_test
import Signature_test
import ModelChecker_test

main = do testcheckProofStep
          testcheckTree
          testcheckProof
          testSignature
          testModelChecker
          return ()



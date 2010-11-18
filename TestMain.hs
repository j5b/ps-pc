{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: mparusinski@googlemail.com	
   License: GPL 3.0
   File: TestMain.hs
   Description: Perform all the tests
-}

module TestMain (runTestsPSPC) where

-- import Proof_test
import ProofChecker_test
import Signature_test
import ModelChecker_test
import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

runTestsPSPC :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTestsPSPC args expected description buildinfo = do testcheckProofStep
                                                      testcheckTree
                                                      testcheckProof
                                                      testSignature
                                                      testModelChecker
                                                      return ()



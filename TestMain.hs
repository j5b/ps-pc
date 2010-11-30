{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: mparusinski@googlemail.com	
   License: GPL 3.0
   File: TestMain.hs
   Description: Perform all the tests
-}

module TestMain (runTestsPSPC) where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo

import GlobalTests
import ModelChecker_test
import Parser_test
import Proof_test
import ProofChecker_test
import ProofSearch_test
import Signature_test

runTestsPSPC :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTestsPSPC args expected description buildinfo = do allsignaturetests
                                                      allproofdttests
                                                      allmodelcheckertests
                                                      allproofsteptest
                                                      alltreetests
                                                      allproofcheckertests
                                                      allpomutests
                                                      allmodelgentests
                                                      --allglobaltests
                                                      allparsertests
                                                      return ()

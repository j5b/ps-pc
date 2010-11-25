module Main (main) where

import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.PackageDescription
import TestMain

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
        runTests  = runTestPSPC }


module Main (main) where

import Distribution.Simple
import Distribution.Simple.UserHooks
import TestMain

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
        runTests  = runTestsPSPC }

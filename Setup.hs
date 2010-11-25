module Main (main) where

import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.PackageDescription
import TestMain

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
        confHook  = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook.")),
        runTests  = runTestPSPC }


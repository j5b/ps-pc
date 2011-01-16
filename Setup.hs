module Main (main) where

import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import System.Cmd(system)
import System.Exit
--import TestMain

main :: IO ()
main = defaultMain

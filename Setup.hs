module Main (main) where

import Distribution.Simple
import Distribution.Simple.UserHooks
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import System.Cmd(system)
import System.Exit
--import TestMain

postconfiguration :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postconfiguration _ _ _ _ -- ignores everything
    = do checklatex
         checkgraphivz

checklatex :: IO ()
checklatex = do putStrLn "checking for latex"
                latexcode <- system "pdflatex --version"
                if latexcode /= ExitSuccess 
                   then error "Make sure latex is installed"
                   else return ()

checkgraphivz :: IO ()
checkgraphivz = do putStrLn "checking for graphviz"
                   graphivzcode <- system "dot --version"
                   if graphivzcode /= ExitSuccess
                      then error "Make sure graphivz is installed with the program dot included"
                      else return ()

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
     { postConf = postconfiguration }


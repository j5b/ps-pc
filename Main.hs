{- 
   Author: Jannis
   License: GPL 3.0
   File: Main.hs
   Description: The main file.
-}

{-
   Here talk about how to deal with IO, parsing, blah.
   Also contains some generally useful functions.
-}

import Data.Either
import Data.List
import System.IO
import System(getArgs)

import Model
import ModelChecker
import Parser
import Proof
import ProofChecker
import ProofSearch
import Signature
import Reader

-- TODO: parse input and call the proof/model searcher and check the result
--main :: IO()
main = do input <- getArgs
          info <- processArgs input
--          putStrLn $ show info
          command <- createCommand info
          executeCommand command
          return ()

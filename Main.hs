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
-- import System.IO
-- import System(getArgs)

import Model
import ModelChecker
import Proof
import ProofChecker
import ProofSearch
import Signature

-- TODO: parse input and call the proof/model searcher and check the result
--main :: IO()
main = do putStr $ "Nothing happening " ++ "at "
          putStr $ "the " ++ "moment\n"

-- Dispatches call to either the model or proof checker.
-- TODO: Make interfaces compatible.
check :: [Concept] -> [Concept] -> Either Model ProofTree -> (String, Bool)
-- TODO: Need model checker that can deal with knowledge base and a set of
--       concepts to dispatch this call.
check cs gamma (Left model) = checkInputModel model gamma cs
check _ gamma (Right proof) = checkProof proof gamma


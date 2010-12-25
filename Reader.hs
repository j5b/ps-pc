{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: Interpreter.hs
   Main functions for dealing with an interpreter
-}

module Reader where

import Data.Maybe
import Control.Monad

import Parser
import ConsoleOutput
import Model
import Proof
import ProofSearch
import Signature
import OutputModel
import OutputProof

type Gamma = [Concept]
type Givens = [Concept]
type Data = (Gamma, Givens)

data OutputMode = None | Console | Graphical -- to be modified later
  deriving (Show, Eq)
data Command = Solve OutputMode Data String | Help
  deriving (Show, Eq)

helpString :: String
helpString = unlines ["The syntax used by the program is",
                      " Forall stands for \"forall\"",
                      " Exists stands for \"exists\"",
                      "  ~     stands for \"not\"",
                      "  &     stands for \"and\"",
                      "  |     stands for \"or\"",
                      " bot    stands for \"falsity\"",
                      " top    stands for \"truth\"",
                      "---------------------------------",
                      "The input for the program is OUTPUT GAMMA CONCEPTS (FILENAME)",
                      " where OUTPUT = none | console | graphical",
                      " and gamma and concepts is a list of formulaes"]

-- output the result of the proof/model searcher
outputResult :: OutputMode -> Either Model ProofTree -> String -> IO ()
outputResult None result _             = either left right result
  where left  x = putStrLn "SATISFIABLE"
        right x = putStrLn "UNSATISFIABLE"
outputResult Console result _          = putStr $ resultToConsole result
outputResult Graphical result filename = either left right result
  where left  x = do outputModel x filename "png" 
                     putStrLn $ "A generated model has been outputed to models/"++filename++".png"
        right x = do outputProof x $ "proofs/"++filename
                     putStrLn $ "A generated proof has been outputed to proofs/"++filename++".pdf"

-- executes command given
executeCommand :: Command -> IO ()
executeCommand (Solve mode (gamma, givens) filename )
  = do putStrLn $ "For GAMMA: "++conceptsToConsole gamma
       putStrLn $ "For GIVENS: "++conceptsToConsole givens
       if mode /= Graphical 
          then putStrLn "The result is:"
          else putStrLn "The result will be generated in a seprate file"
       outputResult mode (findPOM givens gamma) filename
executeCommand Help
  = putStrLn helpString

processInput :: [String] -> IO ()
processInput (x:xs) = processString x xs

-- from a well formed string creates a command
-- Mode is already known before hand
processString :: String -> [String] -> IO ()
processString [] _  = error "No arguments provided"
processString ('-':'h':rest) _
  = executeCommand Help
processString ('-':'-':'h':'e':'l':'p':' ':rest) _
  = executeCommand Help
processString string args
  | numOfArgs < 2  = error "Not enough arguments"
  | numOfArgs == 2 = executeCommand (Solve mode concepts "output")
  | numOfArgs > 3  = error "Too many arguments"
  | otherwise      = executeCommand (Solve mode concepts filename)
  where (mode, rest) = process string
            where process :: String -> (OutputMode, String)
                  process ('n':'o':'n':'e':rest) = (None, rest)
                  process ('c':'o':'n':'s':'o':'l':'e':rest) = (Console, rest)
                  process ('g':'r':'a':'p':'h':'i':'c':'a':'l':rest) = (Graphical, rest)
                  process _ = error "Unable to process input"
        numOfArgs = length args
        gamma     = head args
        givens    = args !! 1
        filename  = args !! 2
        concepts  = (if gamma  == []   then [] else file $ lexerI gamma, 
                     if givens == "  " then [] else file $ lexerI givens)

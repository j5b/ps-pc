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

data OutputMode = None | Console | Graphical
                  deriving (Show, Eq)
                 
data GammaInput  = DirectGamma String | FileGamma FilePath
                   deriving (Show, Eq)
data GivensInput = DirectGivens String | FileGivens FilePath
                   deriving (Show, Eq)
data OutputFile  = DefaultLocation | Location FilePath
                   deriving (Show, Eq)

data Info = Gamma GammaInput | Givens GivensInput
          | OutFile OutputFile | OutMode OutputMode
          | HelpRequest
            deriving (Show, Eq)

data Command = Solve OutputMode Data FilePath | Help
  deriving (Show, Eq)

readLocation :: OutputFile -> String
readLocation DefaultLocation = "output"
readLocation (Location file) = file

readGamma :: GammaInput -> IO String
readGamma (DirectGamma gamma) = return gamma
readGamma (FileGamma file) 
    = do content <- readFile file
         return $ filter (\x -> not $ elem x "\n\t\b") content

readGivens :: GivensInput -> IO String
readGivens (DirectGivens givens) = return givens
readGivens (FileGivens file) 
    = do content <- readFile file
         return $ filter (\x -> not $ elem x "\n\t\b") content

getMode :: String -> OutputMode
getMode ('n':'o':'n':'e':rest) = None
getMode ('c':'o':'n':'s':'o':'l':'e':rest) = Console
getMode ('g':'r':'a':'p':'h':'i':'c':'a':'l':rest) = Graphical
getMode _ 
    = error "Unable to process mode: please write 'none', 'console' or 'graphical'"


-- is str1 in str2 or vice versa partially
match :: String -> String -> Bool
match str1 str2 = match' str1 str2
    where match' [] _ = True
          match' _ [] = True 
          match' (x:xs) (y:ys) 
                 | x == y    = match' xs ys
                 | otherwise = False

-- Some way of determining if we are working with a file path
isFilePath :: String -> Bool
isFilePath string = length (filter (== '.') string) == 1

createCommand :: [Info] -> IO Command
createCommand [] = error "Can't do nothing with no arguments please refer to help"
createCommand infos
    | HelpRequest `elem` infos = return Help
    | otherwise = do 
        gammaStr <- readGamma gm
        givensStr <- readGivens gi
        let gamma = file $ lexerI gammaStr
        let givens = file $ lexerI givensStr
        return $ Solve outmode (gamma,givens) outfile
    where (gm,gi,outf,outmode) = processInfo infos
          outfile = readLocation outf

processInfo :: [Info] -> (GammaInput, GivensInput, OutputFile, OutputMode)
processInfo [] = error "Unexpected error in processInfo, please contact developer"
processInfo [Gamma gamma] 
    = (gamma, DirectGivens "", DefaultLocation, None)
processInfo [Givens givens]
    = (DirectGamma "", givens, DefaultLocation, None)
processInfo [OutFile outfile]
    = (DirectGamma "", DirectGivens "", outfile, None)
processInfo [OutMode mode]
    = (DirectGamma "", DirectGivens "", DefaultLocation, mode)
processInfo (Gamma gamma:rest)
    = (gamma,gi,outf,outm)
    where (gm,gi,outf,outm) = processInfo rest
processInfo (Givens givens:rest)
    = (gm,givens,outf,outm)
    where (gm,gi,outf,outm) = processInfo rest
processInfo (OutFile outfile:rest)
    = (gm,gi,outfile,outm)
    where (gm,gi,outf,outm) = processInfo rest
processInfo (OutMode mode:rest)
    = (gm,gi,outf,mode)
    where (gm,gi,outf,outm) = processInfo rest

-- Takes the arguments a create a list of infos :)
processArgs :: [String] -> IO [Info]
processArgs []    = return []
processArgs [arg] 
    | match arg "-h" = return [HelpRequest]
    | match arg "--help" = return [HelpRequest]
    | otherwise = error "Couldn't understand argument: Did you mean -h or --help?"
processArgs (option:matching:rest)
    | match option "-h" = do 
         putStrLn "Too many arguments: Dropping them and outputing help information"
         return [HelpRequest]
    | match option "--help" = do 
         putStrLn "Too many arguments: Dropping them and outputing help information"
         return [HelpRequest]
    | match option "--gamma"  = do 
         restinfo <- processArgs rest
         if isFilePath matching 
             then return $ Gamma (FileGamma matching):restinfo
             else return $ Gamma (DirectGamma matching):restinfo
    | match option "--givens" = do 
         restinfo <- processArgs rest 
         if isFilePath matching
             then return $ Givens (FileGivens matching):restinfo
             else return $ Givens (DirectGivens matching):restinfo
    | match option "--output" = do 
         restinfo <- processArgs rest
         return $ OutFile (Location matching):restinfo
    | match option "--mode" = do 
         restinfo <- processArgs rest
         return $ OutMode (getMode matching):restinfo
    | otherwise = error $ "Unable to understand argument "++option++" please refer to help"

helpString :: IO String
helpString = readFile "HELP.TXT"

-- output the result of the proof/model searcher
outputResult :: OutputMode -> Either Model ProofTree -> FilePath -> IO ()
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
executeCommand (Solve mode (gamma, givens) filename)
  = do putStrLn $ "For GAMMA: "++conceptsToConsole gamma
       putStrLn $ "For GIVENS: "++conceptsToConsole givens
       if mode /= Graphical 
          then putStrLn "The result is:"
          else putStrLn "The result will be generated in a seperate file (indicated by --output if specified)"
       outputResult mode (findPOM givens gamma) filename
executeCommand Help
  = do output <- helpString
       putStrLn output

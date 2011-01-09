{-
  Author : Michal Parusinski
  EMail : <mparusinski@googlemail.com>
  License: GPLv3
-}

module ConsoleOutput where 

import Data.List
import Data.Either

import TestUtils
import Signature
import Proof
import Model
import ProofSearch

topConsole = "top"
botConsole = "bot"
andConsole = "&"
orConsole  = "|"
negConsole = "~"
existsConsole = "Exists"
forallConsole = "Forall"

-- creates a string representing the model or the proof tree
resultToConsole :: Either Model ProofTree -> FilePath -> IO ()
resultToConsole (Left model) file = modelToConsole file model
resultToConsole (Right proof) file = proofToConsole file proof

-- creates a string representing the model
modelToConsole :: FilePath -> Model -> IO ()
modelToConsole file (dom,unarys,binarys) 
    = do putStrLn $ "The generated model has been saved to models/"++file++".txt"
         let result = dompart++unarypart++binpart
         writeFile ("models/"++file++".txt") result
         putStrLn result
    where dompart = "Domain  = "++show dom++"\n"
          unarypart = if unarys == [] 
                      then "No unary relation\n" 
                      else "Unarys  = \n"++processUnarys unarys
          binpart   = if binarys == [] 
                      then "No binary relation\n" 
                      else "Binarys = \n"++processBinarys binarys

-- Utility function for modelToConsole
processUnarys :: Num a => [(String,[a])] -> String
processUnarys list = concatMap f list
   where f (unary, space) = "--Unary "++unary++" is satisfied for: "++show space++"\n"

-- Utility function for modelToConsole
processBinarys :: Num a => [(String,[(a,a)])] -> String
processBinarys list = concatMap f list
   where f (binary, space) = "--Binary "++binary++" is satisfied for: "++show space++"\n"

-- creates a string representing the proof
proofToConsole :: FilePath -> ProofTree -> IO ()
proofToConsole file proof
    = do let result = (unlines . proofToConsoleInternal) proof
         putStrLn $ "The generated proof has been saved to proofs/"++file++".txt"
         writeFile ("proofs/"++file++".pdf") result
         putStrLn result

proofToConsoleInternal :: ProofTree -> [String]
proofToConsoleInternal (NodeZero (cs,rule,concept))
  = [from,by,to]
  where from = conceptsToConsole cs
        by   = "=Contradiction on: "++conceptToConsole concept
        to   = "--UNSAT--"
proofToConsoleInternal (NodeOne (cs,rule,concept) rest)
  = [from,by]++to
  where from = conceptsToConsole cs
        by   = "=By rule \'"++rule++"\' applied on concept "++conceptToConsole concept
        to   = proofToConsoleInternal rest
proofToConsoleInternal (NodeTwo (cs,rule,concept) left right)
  = [from,by]++leftpart++rightpart
  where from = conceptsToConsole cs
        by   = "=By rule \'"++rule++"\' applied on concept "++conceptToConsole concept
        leftpart  = ["STARTLEFT"]++map (\x->"  | "++x) repLeft++["ENDLEFT"]
        rightpart = ["STARTRIGHT"]++map (\x->"  | "++x) repRight++["ENDRIGHT"]
        repLeft   = proofToConsoleInternal left
        repRight  = proofToConsoleInternal right

conceptsToConsole :: [Concept] -> String
conceptsToConsole [] = ""
conceptsToConsole [concept] = conceptToConsole concept
conceptsToConsole (c:cs) = conceptToConsole c++rest
  where rest = concatMap (\x -> ',':conceptToConsole x) cs

-- Turns a concept into a nice string (for console output)
conceptToConsole :: Concept -> String
conceptToConsole T = topConsole
conceptToConsole (Neg T) = botConsole
conceptToConsole (Atom a)  = a
conceptToConsole (Neg (Atom a)) = negConsole++a
conceptToConsole (Neg concept) = negConsole++"("++conceptToConsole concept++")"
conceptToConsole (Or (Atom a) (Atom b)) = a++orConsole++b
conceptToConsole (Or T (Neg T)) = topConsole++orConsole++botConsole
conceptToConsole (Or (Neg T) T) = botConsole++orConsole++topConsole
conceptToConsole (Or T (Atom b)) = topConsole++orConsole++b
conceptToConsole (Or (Neg T) (Atom b)) = botConsole++orConsole++b
conceptToConsole (Or (Atom a) T) = a++orConsole++topConsole
conceptToConsole (Or (Atom a) (Neg T)) = a++orConsole++botConsole
conceptToConsole (Or (Atom a) rc) = a++orConsole++"("++conceptToConsole rc++")"
conceptToConsole (Or T rc) = topConsole++orConsole++"("++conceptToConsole rc++")"
conceptToConsole (Or (Neg T) rc) =botConsole++orConsole++"("++conceptToConsole rc++")"
conceptToConsole (Or lc (Atom a)) = "("++conceptToConsole lc++")"++orConsole++a
conceptToConsole (Or lc T) = "("++conceptToConsole lc++")"++orConsole++topConsole
conceptToConsole (Or lc (Neg T)) = "("++conceptToConsole lc++")"++orConsole++botConsole
conceptToConsole (Or lc rc) = "("++conceptToConsole lc++")"++orConsole++"("++conceptToConsole rc++")"
conceptToConsole (And (Atom a) (Atom b)) = a++andConsole++b
conceptToConsole (And T (Neg T)) = topConsole++andConsole++botConsole
conceptToConsole (And (Neg T) T) = botConsole++andConsole++botConsole
conceptToConsole (And T (Atom b)) = topConsole++andConsole++b
conceptToConsole (And (Neg T) (Atom b)) = botConsole++andConsole++b
conceptToConsole (And (Atom a) T) = a++andConsole++topConsole
conceptToConsole (And (Atom a) (Neg T)) = a++andConsole++botConsole
conceptToConsole (And (Atom a) rc) = a++andConsole++"("++conceptToConsole rc++")"
conceptToConsole (And T rc) = topConsole++andConsole++"("++conceptToConsole rc++")"
conceptToConsole (And (Neg T) rc) = botConsole++andConsole++"("++conceptToConsole rc++")"
conceptToConsole (And lc (Atom a)) = "("++conceptToConsole lc++")"++andConsole++a
conceptToConsole (And lc T) = "("++conceptToConsole lc++")"++andConsole++topConsole
conceptToConsole (And lc (Neg T)) = "("++conceptToConsole lc++")"++andConsole++botConsole
conceptToConsole (And lc rc) = "("++conceptToConsole lc++")"++andConsole++"("++conceptToConsole rc++")"
conceptToConsole (Exists rel (Atom a)) = existsConsole++" "++rel++" ("++a++")"
conceptToConsole (Exists rel T) = existsConsole++" "++rel++" ("++topConsole++")"
conceptToConsole (Exists rel (Neg T)) = existsConsole++" "++rel++" ("++botConsole++")"
conceptToConsole (Exists rel concept) = existsConsole++" "++rel++" ("++conceptToConsole concept++")"
conceptToConsole (Forall rel (Atom a)) = forallConsole++" "++rel++" ("++a++")"
conceptToConsole (Forall rel T) = forallConsole++" "++rel++" ("++topConsole++")"
conceptToConsole (Forall rel (Neg T)) = forallConsole++" "++rel++" ("++botConsole++")"
conceptToConsole (Forall rel concept) = forallConsole++" "++rel++" ("++conceptToConsole concept++")"

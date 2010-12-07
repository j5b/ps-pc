{- 
   Maintainer: Michal Parusinski
   Email: <mparusinski@googlemail.com
   License: GPL 3.0
   File: OutputProof_test.hs
   Description: This will actually generate a latex file :)
-}

module OutputProof_test where

import Signature
import OutputProof
import TestUtils
import ProofSearch

import System.IO
import System.Process
import System.Exit
import Control.Monad
import Test.HUnit

myConceptList = generateConcepts 4 :: [Concept]

elementOutput concept = "$"++conceptToLatex concept++"$ \\ \n"

fileC2LGenerator :: IO ()
fileC2LGenerator = do output <- openFile "concept_latex_test_output.tex" WriteMode
                      hPutStrLn output "\\documentclass[12pt, a4paper]{article}"
                      hPutStrLn output "\\usepackage{amsmath}"
                      hPutStrLn output "\\usepackage{amssymb}"
                      hPutStrLn output "\\begin{document}"
                      mapM (hPutStrLn output) $ map elementOutput myConceptList
                      hPutStrLn output "\\bigbreak \\ \n"
                      hPutStrLn output ("$"++conceptsToLatex myConceptList++"$")
                      hPutStrLn output "\\end{document}"
                      hClose output
                      putStrLn "\nFile for testing conceptToLatex function has been generated"

createC2LPDF :: IO ExitCode
createC2LPDF = do fileC2LGenerator 
                  hProcess <- runCommand "pdflatex -halt-on-error -interaction=nonstopmode concept_latex_test_output.tex > /dev/null"
                  waitForProcess hProcess

testC2L = TestCase (test)
  where test = do code <- createC2LPDF
                  unless (code == ExitSuccess) fail 
        fail = assertFailure "Failed to compile simple concept representation in latex"

fileTreeGenerate :: IO ()
fileTreeGenerate = do output <- openFile "tree_test_output.tex" WriteMode
                      hPutStrLn output "\\documentclass[12pt, a4paper]{article}"
                      hPutStrLn output "\\usepackage{amsmath}"
                      hPutStrLn output "\\usepackage{amssymb}"
                      hPutStrLn output "\\usepackage{qtree}"
                      hPutStrLn output "\\begin{document}"
                      mapM (hPutStrLn output) inputlist
                      hPutStrLn output "\\end{document}"
                      hClose output
                      putStrLn "\nFile for testing output proofs has been generated"
  where list = map (flip findPOM []) $ map return myConceptList
        inputlist = map (either (\x -> "") (\x -> "\\Tree\n"++latexify x++"\n\\bigbreak\n")) list
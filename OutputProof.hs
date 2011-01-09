{- 
   Author: Michal Parusinski
   Email: mparusinski@googlemail.com
   License: GPL 3.0
   File: OutputProof.hs
   Description: outputs a latex file that can be inserted in a latex document
-}

module OutputProof where 

import System.IO
import System.Cmd(system)
import System.Exit
import Control.Monad

import Signature
import Proof
import ProofSearch

outputProof :: ProofTree -> FilePath -> IO ()
outputProof proof filename 
  = do createGenericPDF [proof] $ filename++".tex"
       let command = "pdflatex -halt-on-error -interaction=nonstopmode "++
                     filename++".tex > /dev/null"
       system command
       return ()

getConcepts (cs,_,_) = cs
getRule (_,rule,_) = rule
getUsed (_,_,used) = used

latexify :: ProofTree -> String
latexify (NodeZero step) = "[.{"++concepts++end++"\n]"
  where concepts = niceConceptLatex (getConcepts step)
        end      = "}\n\t[. { $\\bot$ } ]"
latexify (NodeOne step tree) = "[.{"++concepts++"}\n\t"++rest++"\n]"
  where concepts = niceConceptLatex (getConcepts step)
        rule     = " ("++getRule step++") " -- ++" for $"++(conceptToLatex $ getConceptUsed step)++"$) "
        rest     = latexify tree
latexify (NodeTwo step left right) = "[.{"++concepts++"}\n\t"++lrest++"\n\t"++rrest++"\n]"
  where concepts = niceConceptLatex (getConcepts step)
        rule     = " ("++getRule step++") " -- ++" for $"++(conceptToLatex $ getConceptUsed step)++"$) "
        lrest    = latexify left
        rrest    = latexify right

proofToLatexTree :: ProofTree -> String
proofToLatexTree prooftree = "\\Tree\n"++latexify prooftree++"\n"

createGenericPDF :: [ProofTree] -> FilePath -> IO ()
createGenericPDF proofs file = do putStrLn $ "Opening file "++file
                                  output <- openFile file WriteMode
                                  hPutStrLn output header
                                  mapM_ (hPutStrLn output . proofToLatexTree) proofs
                                  hPutStrLn output end
                                  putStrLn $ "Closing file "++file
                                  hClose output
  where header = "\\documentclass[8pt, a4paper]{article}\n"++
                 "\\usepackage{amsmath}\n"++
                 "\\usepackage{amssymb}\n"++
                 "\\usepackage{qtree}\n"++
                 "\\usepackage{fullpage}\n"++
                 "\\begin{document}\n"++
                 "\\begin{center}\n"
        end    = "\\end{center}\n"++"\\end{document}"

breakInPieces _ [] = []
breakInPieces size list = result:breakInPieces size rest
  where (result,rest) = countTo 0 size list
        countTo _ _ [] = ([],[])
        countTo num size (x:xs) 
            | num >= size = ([],x:xs)
            | otherwise   = (newleft, newright)
            where (oldleft, oldright) = countTo (num+conceptLength x) size xs
                  newleft             = x:oldleft
                  newright            = oldright

-- Outputs in a nice formated format for latex to improve readability
niceConceptLatex :: [Concept] -> String
niceConceptLatex cs = concatMap fullFormat (init conceptsList) ++ lastFormat (last conceptsList)
  where conceptsList = breakInPieces 6 cs
        fullFormat list = "$"++conceptsToLatex list++",$\\\\"
        lastFormat list = "$"++conceptsToLatex list++"$"

-- Way to mesure how long a concept is so we can break out the proof more neatly
-- this estimates how a formula will be in latex
conceptLength T              = 1
conceptLength (Neg T)        = 1
conceptLength (Atom a)       = length a
conceptLength (Neg concept)  = 1+conceptLength concept
conceptLength (Or c1 c2)     = 1+conceptLength c1+conceptLength c2
conceptLength (And c1 c2)    = 1+conceptLength c1+conceptLength c2
conceptLength (Exists rel c) = conceptLength c+length rel+1
conceptLength (Forall rel c) = conceptLength c+length rel+1

-- Turns a concept into a nice string (for latex)
conceptToLatex :: Concept -> String
conceptToLatex T = "\\top"
conceptToLatex (Neg T) = "\\bot"
conceptToLatex (Atom a)  = a
conceptToLatex (Neg (Atom a)) = "\\neg "++a
conceptToLatex (Neg concept) = "\\neg ("++conceptToLatex concept++")"
conceptToLatex (Or (Atom a) (Atom b)) = a++" \\lor "++b
conceptToLatex (Or T (Neg T)) = "\\top \\lor \\bot"
conceptToLatex (Or (Neg T) T) = "\\bot \\lor \\top"
conceptToLatex (Or T (Atom b)) = "\\top \\lor "++b
conceptToLatex (Or (Neg T) (Atom b)) = "\\bot \\lor "++b
conceptToLatex (Or (Atom a) T) = a++" \\lor \\top"
conceptToLatex (Or (Atom a) (Neg T)) = a++" \\lor \\bot"
conceptToLatex (Or (Atom a) rc) = a++" \\lor ("++conceptToLatex rc++")"
conceptToLatex (Or T rc) = "\\top \\lor ("++conceptToLatex rc++")"
conceptToLatex (Or (Neg T) rc) ="\\bot \\lor ("++conceptToLatex rc++")"
conceptToLatex (Or lc (Atom a)) = "("++conceptToLatex lc++") \\lor "++a
conceptToLatex (Or lc T) = "("++conceptToLatex lc++") \\lor \\top"
conceptToLatex (Or lc (Neg T)) = "("++conceptToLatex lc++") \\lor \\bot"
conceptToLatex (Or lc rc) = "("++conceptToLatex lc++") \\lor ("++conceptToLatex rc++")"
conceptToLatex (And (Atom a) (Atom b)) = a++" \\land "++b
conceptToLatex (And T (Neg T)) = "\\top \\land \\bot"
conceptToLatex (And (Neg T) T) = "\\bot \\land \\top"
conceptToLatex (And T (Atom b)) = "\\top \\land "++b
conceptToLatex (And (Neg T) (Atom b)) = "\\bot \\land "++b
conceptToLatex (And (Atom a) T) = a++" \\land \\top"
conceptToLatex (And (Atom a) (Neg T)) = a++" \\land \\bot"
conceptToLatex (And (Atom a) rc) = a++" \\land ("++conceptToLatex rc++")"
conceptToLatex (And T rc) = "\\top \\land ("++conceptToLatex rc++")"
conceptToLatex (And (Neg T) rc) = "\\bot \\land ("++conceptToLatex rc++")"
conceptToLatex (And lc (Atom a)) = "("++conceptToLatex lc++") \\land "++a
conceptToLatex (And lc T) = "("++conceptToLatex lc++") \\land \\top"
conceptToLatex (And lc (Neg T)) = "("++conceptToLatex lc++") \\land \\bot"
conceptToLatex (And lc rc) = "("++conceptToLatex lc++") \\land ("++conceptToLatex rc++")"
conceptToLatex (Exists rel (Atom a)) = "\\exists "++rel++". "++a
conceptToLatex (Exists rel T) = "\\exists "++rel++". \\top"
conceptToLatex (Exists rel (Neg T)) = "\\exists "++rel++". \\bot"
conceptToLatex (Exists rel concept) = "\\exists "++rel++". ("++conceptToLatex concept++")"
conceptToLatex (Forall rel (Atom a)) = "\\forall "++rel++". "++a
conceptToLatex (Forall rel T) = "\\forall "++rel++". \\top"
conceptToLatex (Forall rel (Neg T)) = "\\forall "++rel++". \\bot"
conceptToLatex (Forall rel concept) = "\\forall "++rel++". ("++conceptToLatex concept++")"

conceptsToLatex :: [Concept] -> String
conceptsToLatex [] = ""
conceptsToLatex [concept] = conceptToLatex concept
conceptsToLatex list = iterator list
  where iterator [] = "" -- this shoudln't occur
        iterator [concept] = conceptToLatex concept
        iterator (c:cs)    = conceptToLatex c++","++conceptsToLatex cs

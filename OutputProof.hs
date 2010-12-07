{- 
   Author: Michal Parusinski
   Email: mparusinski@googlemail.com
   License: GPL 3.0
   File: OutputProof.hs
   Description: outputs a latex file that can be inserted in a latex document
-}

module OutputProof where 

import Signature
import Proof
import ProofSearch

import TestUtils

latexify :: ProofTree -> String
latexify (NodeZero step) = "[.{"++concepts++end++"\n]"
  where concepts = "$"++(conceptsToLatex $ getConcepts step)++"$"
        end      = "}\n\t[. { $\\ast$ } ]"
latexify (NodeOne step tree) = "[.{"++concepts++"}\n\t"++rest++"\n]"
  where concepts = "$"++(conceptsToLatex $ getConcepts step)++"$"
        rule     = " ("++getRule step++") " -- ++" for $"++(conceptToLatex $ getConceptUsed step)++"$) "
        rest     = latexify tree
latexify (NodeTwo step left right) = "[.{"++concepts++"}\n\t"++lrest++"\n\t"++rrest++"\n]"
  where concepts = "$"++(conceptsToLatex $ getConcepts step)++"$"
        rule     = " ("++getRule step++") " -- ++" for $"++(conceptToLatex $ getConceptUsed step)++"$) "
        lrest    = latexify left
        rrest    = latexify right

-- Turns a concept into a nice string (for latex)
conceptToLatex :: Concept -> String
conceptToLatex T = "\\top"
conceptToLatex (Neg T) = "\\bot"
conceptToLatex (Atom a)  = a
conceptToLatex (Neg (Atom a)) = "\\neg "++a
conceptToLatex (Neg concept) = "\\neg ("++conceptToLatex concept++")"
conceptToLatex (Or (Atom a) (Atom b)) = a++" \\lor "++b
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
conceptToLatex (And T (Atom b)) = "\\top \\land "++b
conceptToLatex (And (Neg T) (Atom b)) = "\\bot \\land "++b
conceptToLatex (And (Atom a) T) = a++" \\land \\top"
conceptToLatex (And (Atom a) (Neg T)) = a++" \\land \\top"
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
conceptToLatex (Forall rel concept) = "\\exists "++rel++". ("++conceptToLatex concept++")"

conceptsToLatex :: [Concept] -> String
conceptsToLatex [] = ""
conceptsToLatex [concept] = conceptToLatex concept
conceptsToLatex list = iterator list
  where iterator [] = "" -- this shoudln't occur
        iterator [concept] = conceptToLatex concept
        iterator (c:cs) = conceptToLatex c++","++conceptsToLatex cs

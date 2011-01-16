{- 
   Author: Computer generated/Michal Parusinski
   Maintainer: Michal Parusinski
   Email: mparusinski@googlemail.com
   License: GPL 3.0
   Description: Help text for --help option
-}

module HelpText where

help = "ps-pc --help\nps-pc --givens [GIVENS|FILE] \n      --gamma [GIVENS|FILE] \n      --mode [none|console|graphical|check]\n      --output [NAME]\n\n--DESCRIPTION:\n\nThis program searches models for a given knowledge base and a set of\nconcepts formulated in Description Logic.  If no such model can be\nfound a proof is provided.\n\n--LOGIC SYNTAX:\n\nThe syntax for Description Logic used is the following:\n  'Forall' -> universal quantification: Forall Relation (Concept)\n  'Exists' -> existential quantification: Exists Relation (Concept)\n  'top'    -> Top/Truth/Tautology (always true): top\n  'bot'    -> Bottom/Falsity (always false): bot\n  '&'      -> Intersection/And symbol: (concept1)&(concept2)\n  '|'      -> Union/Or symbol: (concept1)|(concept2)\n  '~'      -> Complement/Negation symbol: ~(concept)\n  '->'     -> Implication: (concept1)->(concept2)\n  '=='     -> Equivalence: (concept1)==(concept2)\n  ';' to seperate Concepts (Formulaes) in a list: concept1;concept2\nThe rest represents atomic or binary relation (used in forall and exists).\nBrackets can be omitted only if there is no doubt about the meaning.\nThe concepts can be given described in a file. Tabs, spaces and newlines are\nhandled.\n\ne.g. Forall R (A|top);~(~(top));A|(A|(B&Exists S (top|bot)))\n\n--OPTIONS\n\nThere are the following options for this program\n  --help for displaying this message\n  --gamma for specifying gamma (in a file or directly)\n         default is no concepts\n  --givens for specifying givens (in a file or directly)\n         default is no concepts\n  --mode for specifying what type of output you wish\n         -> none for simply stating SATISFIABLE or UNSATISFIABLE\n         -> console for a verbose console output of the model or proof\n         -> graphical for a verbose output in a file of the model or proof\n\t -> check for a verbose console output and the program will check the result with \n\t    the model or proof checker\n         default is 'none'\n  --output for specifying the output file name (only makes sense for graphical and console\n  outputs) \n         default is 'output'\n\nThe order for the options do not matter. Help option discards all the other \ninformation.\n\nFiles has to be of the form BASENAME.EXTENSION\n\ne.g. ps-pc --output sample --gamma gamma.txt \n           --givens \"Intelligent|Smart\" --mode graphical\n"
{
{- 
   Author: Jannis, Ka Wai
   License: GPL 3.0
   File: ParserBenchmark1.y/hs
   Description: The generated concept parser for user input and benchmarks from
                http://iamwww.unibe.ch/~lwb/benchmarks/benchmarks.htm,
                http://www.cs.man.ac.uk/~schmidt/mspass/problems.html.
-}

module Parser where

import Data.Char
import Signature
}

%name file
%tokentype { Token }
%error { parseError }

%token 
      begin           { TokenBegin }
      end             { TokenEnd }
      var             { TokenVar $$ }
      rel             { TokenRel $$ }
      true            { TokenTrue }
      false           { TokenFalse }
      and             { TokenAnd }
      or              { TokenOr }
      '->'            { TokenImplies }
      not             { TokenNeg }
      Forall          { TokenForall }
      Exists          { TokenExists }
      ';'             { TokenSemicolon }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

File     : begin ConceptSeq end     { $2 }
         | ConceptSeq               { $1 }

ConceptSeq
         : ConceptSeq ';' Concept     { $1 ++ [$3] }
         | Concept                    { [$1] }

Concept  : Concept '->' Concept1       {Or (Neg $1) $3}
         | Concept and Concept1        {And $1 $3}
         | Concept or Concept1         {Or $1 $3}
         | and '(' Concept Concept ')' {And $3 $4}
         | or '(' Concept Concept ')'  {Or $3 $4}
         | Concept1                    {$1}

Concept1 : Forall rel '(' Concept ')' {Forall $2 $4}
         | Exists rel '(' Concept ')' {Exists $2 $4}
         | not Concept2               {Neg $2}
         | Concept2                   {$1}

Concept2 : var                      {Atom $1}
         | true                     {top}
         | false                    {bottom}
         | '(' Concept ')'          {$2}
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenTrue
      | TokenFalse
      | TokenVar String
      | TokenRel String
      | TokenAnd
      | TokenOr
      | TokenImplies
      | TokenNeg
      | TokenForall
      | TokenExists
      | TokenSemicolon
      | TokenOB
      | TokenCB
      | TokenBegin
      | TokenEnd
 deriving Show

-- Top level parser, first argument defines which parser to use
lexer :: String -> String -> [Token]
lexer "Benchmark1" s = lexerB1 s
lexer "Benchmark2" s = lexerB2 s
lexer "Input"      s = lexerI s
lexer _            _ = parseError []

-- Lexer for input grammar.
lexerI :: String -> [Token]
lexerI [] = []
lexerI ('F':'o':'r':'a':'l':'l':cs) = TokenForall    : lexIRel cs
lexerI ('E':'x':'i':'s':'t':'s':cs) = TokenExists    : lexIRel cs
lexerI ('t':'o':'p':cs)             = TokenTrue      : lexerI cs
lexerI ('b':'o':'t':cs)             = TokenFalse     : lexerI cs
lexerI (';':cs)                     = TokenSemicolon : lexerI cs
lexerI ('&':cs)                     = TokenAnd       : lexerI cs
lexerI ('|':cs)                     = TokenOr        : lexerI cs
lexerI ('-':'>':cs)                 = TokenImplies   : lexerI cs
lexerI ('~':cs)                     = TokenNeg       : lexerI cs
lexerI ('(':cs)                     = TokenOB        : lexerI cs
lexerI (')':cs)                     = TokenCB        : lexerI cs
lexerI (c:cs) 
      | isSpace    c = lexerI cs
      | isAlphaNum c = lexIVar (c:cs)

lexIVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexerI rest

lexIRel (' ':cs) = lexIRel cs
lexIRel cs       =
   case span isAlphaNum cs of
      (rel,rest)   -> TokenRel rel : lexerI rest

-- Lexer for Benchmark 1 file
lexerB1 :: String -> [Token]
lexerB1 [] = []
lexerB1 ('b':'e':'g':'i':'n':cs)
             = TokenBegin : lexerB1Concepts cs
lexerB1 (c:cs) = lexerB1 cs

lexerB1Concepts :: String -> [Token]
lexerB1Concepts [] = []
lexerB1Concepts ('b':'o':'x':cs) = TokenForall : TokenRel "R" : lexerB1Concepts cs
lexerB1Concepts ('d':'i':'a':cs) = TokenExists : TokenRel "R" : lexerB1Concepts cs
lexerB1Concepts ('&':cs)         = TokenAnd : lexerB1Concepts cs
lexerB1Concepts ('-':'>':cs)     = TokenImplies : lexerB1Concepts cs
lexerB1Concepts ('~':cs)         = TokenNeg : lexerB1Concepts cs
lexerB1Concepts ('(':cs)         = TokenOB : lexerB1Concepts cs
lexerB1Concepts (')':cs)         = TokenCB : lexerB1Concepts cs
-- Only allows benchmark files with less than 10 concepts
lexerB1Concepts ('1':':':cs)     = lexerB1Concepts cs
lexerB1Concepts (_:':':cs)       = TokenSemicolon : lexerB1Concepts cs
lexerB1Concepts ('e':'n':'d':cs) = [TokenEnd]
lexerB1Concepts (c:cs) 
      | isSpace    c = lexerB1Concepts cs
      | isAlphaNum c = lexB1Var (c:cs)

lexB1Var cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexerB1Concepts rest

-- Lexer for Benchmark 2 file
lexerB2 :: String -> [Token]
lexerB2 [] = []
lexerB2 ('b':'e':'g':'i':'n':'_':'p':'r':'o':'b':'l':'e':'m':cs)
               = TokenBegin : lexB2FindConcepts cs
lexerB2 (c:cs) = lexerB2 cs

-- Finds 1st list of concepts
lexB2FindConcepts :: String -> [Token]
lexB2FindConcepts ('l':'i':'s':'t':'_':'o':'f':'_':'s':'p':'e':'c':'i':'a':'l':
                   '_':'f':'o':'r':'m':'u':'l':'a':'e':'(':'c':'o':'n':'j':'e':
                   'c':'t':'u':'r':'e':'s':',':' ':'E':'M':'L':')':'.':cs)
                         = lexerB2Concepts cs
lexB2FindConcepts (c:cs) = lexB2FindConcepts cs

-- Finds next list of Concepts if it exists
lexB2ContConcepts :: String -> [Token]
lexB2ContConcepts ('l':'i':'s':'t':'_':'o':'f':'_':'s':'p':'e':'c':'i':'a':'l':
                   '_':'f':'o':'r':'m':'u':'l':'a':'e':'(':'c':'o':'n':'j':'e':
                   'c':'t':'u':'r':'e':'s':',':' ':'E':'M':'L':')':'.':cs)
                         = TokenSemicolon : lexerB2Concepts cs
lexB2ContConcepts ('e':'n':'d':'_':'p':'r':'o':'b':'l':'e':'m':'.':cs)
                         = [TokenEnd]
lexB2ContConcepts (c:cs) = lexB2ContConcepts cs

-- Parses a list of concepts
lexerB2Concepts :: String -> [Token]
lexerB2Concepts [] = []
lexerB2Concepts ('p':'r':'o':'p':'_':'f':'o':'r':'m':'u':'l':'a':cs)
                       = lexerB2Concept cs
lexerB2Concepts (c:cs) = lexerB2Concepts cs

-- Parses a concept
lexerB2Concept :: String -> [Token]
lexerB2Concept [] = []
lexerB2Concept ('b':'o':'x':'(':cs)     = TokenForall : lexB2Rel cs
lexerB2Concept ('d':'i':'a':'(':cs)     = TokenExists : lexB2Rel cs
lexerB2Concept ('a':'n':'d':cs)         = TokenAnd : lexerB2Concept cs
lexerB2Concept ('o':'r':cs)             = TokenOr : lexerB2Concept cs
lexerB2Concept ('n':'o':'t':cs)         = TokenNeg : lexerB2Concept cs
lexerB2Concept ('t':'r':'u':'e':cs)     = TokenTrue : lexerB2Concept cs
lexerB2Concept ('f':'a':'l':'s':'e':cs) = TokenFalse : lexerB2Concept cs
lexerB2Concept ('(':cs)                 = TokenOB : lexerB2Concept cs
lexerB2Concept (')':cs)                 = TokenCB : lexerB2Concept cs
lexerB2Concept (',':cs)                 = lexerB2Concept cs
lexerB2Concept ('.':cs)                 = lexB2NextConcept cs
lexerB2Concept (c:cs) 
      | isSpace    c = lexerB2Concept cs
      | isAlphaNum c = lexB2Var (c:cs)

-- Parse next concept in a list if it exists
lexB2NextConcept :: String -> [Token]
lexB2NextConcept ('e':'n':'d':'_':'o':'f':'_':'l':'i':'s':'t':'.':cs)
                        = lexB2ContConcepts cs
lexB2NextConcept ('p':'r':'o':'p':'_':'f':'o':'r':'m':'u':'l':'a':cs)
                        = TokenSemicolon : lexerB2Concept cs
lexB2NextConcept (c:cs) = lexB2NextConcept cs

lexB2Rel :: String -> [Token]
lexB2Rel (' ':cs) = lexB2Rel cs
lexB2Rel cs       =
   case span isAlphaNum cs of
      (rel,rest)   -> TokenRel rel : TokenOB : lexerB2Concept rest

lexB2Var cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexerB2Concept rest
}

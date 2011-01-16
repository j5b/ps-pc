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
      var             { TokenVar $$ }
      true            { TokenTrue }
      false           { TokenFalse }
      and             { TokenAnd }
      or              { TokenOr }
      '->'            { TokenImplies }
      '=='            { TokenEquiv }
      not             { TokenNeg }
      Forall          { TokenForall }
      Exists          { TokenExists }
      ';'             { TokenSemicolon }
      '('             { TokenOB }
      ')'             { TokenCB }
      end             { TokenEnd }

%%

File     : ConceptSeq end              { $1 }
         | end                         { [] }

ConceptSeq : ConceptSeq ';' Concept    { $3 : $1 }
         | Concept                     { [$1] }

Concept  : Concept '==' Concept1       {And (Or (Neg $1) $3) (Or $1 (Neg $3))}
         | Concept '->' Concept1       {Or (Neg $1) $3}
         | Concept and Concept1        {And $1 $3}
         | Concept or Concept1         {Or $1 $3}
         | and '(' Concept Concept ')' {And $3 $4}
         | or '(' Concept Concept ')'  {Or $3 $4}
         | Concept1                    {$1}

Concept1 : Forall var '(' Concept ')'  {Forall $2 $4}
         | Exists var '(' Concept ')'  {Exists $2 $4}
         | not Concept2                {Neg $2}
         | Concept2                    {$1}

Concept2 : var                         {Atom $1}
         | true                        {top}
         | false                       {bottom}
         | '(' Concept ')'             {$2}
{

parseError :: [Token] -> a
parseError i = error $ "Parse error on "++show i

data Token
      = TokenTrue
      | TokenFalse
      | TokenVar String
      | TokenAnd
      | TokenOr
      | TokenImplies
      | TokenEquiv
      | TokenNeg
      | TokenForall
      | TokenExists
      | TokenSemicolon
      | TokenOB
      | TokenCB
      | TokenEnd
 deriving Show

-- Returns a TokenVar and rest of string for all the lexers.
lexVar :: String -> (Token, String)
lexVar (' ':cs) = lexVar cs
lexVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> (TokenVar var, rest)

-- Lexer for input grammar.
lexerI :: String -> [Token]
lexerI []                           = [TokenEnd]
lexerI ('F':'o':'r':'a':'l':'l':cs) = TokenForall    : var : lexerI rest
  where (var, rest) = lexVar cs
lexerI ('E':'x':'i':'s':'t':'s':cs) = TokenExists    : var : lexerI rest
  where (var, rest) = lexVar cs
lexerI ('t':'o':'p':cs)             = TokenTrue      : lexerI cs
lexerI ('b':'o':'t':cs)             = TokenFalse     : lexerI cs
lexerI (';':cs)                     = TokenSemicolon : lexerI cs
lexerI ('&':cs)                     = TokenAnd       : lexerI cs
lexerI ('|':cs)                     = TokenOr        : lexerI cs
lexerI ('-':'>':cs)                 = TokenImplies   : lexerI cs
lexerI ('=':'=':cs)                 = TokenEquiv     : lexerI cs
lexerI ('~':cs)                     = TokenNeg       : lexerI cs
lexerI ('(':cs)                     = TokenOB        : lexerI cs
lexerI (')':cs)                     = TokenCB        : lexerI cs
lexerI (c:cs) 
      | isSpace    c = lexerI cs
      | isAlphaNum c = var : (lexerI rest)
  where (var, rest) = lexVar (c:cs)
lexerI other = error other

-- Lexer for Benchmark 1 file.
lexerB1 :: String -> [Token]
lexerB1 [] = [TokenEnd]
lexerB1 ('b':'e':'g':'i':'n':cs)
               = lexB1Concepts cs
lexerB1 (c:cs) = lexerB1 cs

-- Tokenizes a list of concepts.
-- Only allows benchmark files with less than 10 concepts.
lexB1Concepts :: String -> [Token]
lexB1Concepts []               = [TokenEnd]
lexB1Concepts ('b':'o':'x':cs) = TokenForall : TokenVar "R" : lexB1Concepts cs
lexB1Concepts ('d':'i':'a':cs) = TokenExists : TokenVar "R" : lexB1Concepts cs
lexB1Concepts ('&':cs)         = TokenAnd                   : lexB1Concepts cs
lexB1Concepts ('-':'>':cs)     = TokenImplies               : lexB1Concepts cs
lexB1Concepts ('~':cs)         = TokenNeg                   : lexB1Concepts cs
lexB1Concepts ('(':cs)         = TokenOB                    : lexB1Concepts cs
lexB1Concepts (')':cs)         = TokenCB                    : lexB1Concepts cs
lexB1Concepts ('1':':':cs)     = lexB1Concepts cs
lexB1Concepts (_:':':cs)       = TokenSemicolon             : lexB1Concepts cs
lexB1Concepts ('e':'n':'d':cs) = [TokenEnd]
lexB1Concepts (c:cs) 
      | isSpace    c = lexB1Concepts cs
      | isAlphaNum c = var : lexB1Concepts rest
  where (var, rest) = lexVar (c:cs)

-- Lexer for Benchmark 2 file.
lexerB2 :: String -> [Token]
lexerB2 [] = [TokenEnd]
lexerB2 ('l':'i':'s':'t':'_':'o':'f':'_':'s':'p':'e':'c':'i':'a':'l':
         '_':'f':'o':'r':'m':'u':'l':'a':'e':'(':'c':'o':'n':'j':'e':
         'c':'t':'u':'r':'e':'s':',':' ':'E':'M':'L':')':'.':cs)
               = lexB2Concepts cs
lexerB2 (c:cs) = lexerB2 cs

-- Removes rubbish inbetween concepts.
lexB2ContConcepts :: String -> [Token]
lexB2ContConcepts []     = [TokenEnd]
lexB2ContConcepts ('l':'i':'s':'t':'_':'o':'f':'_':'s':'p':'e':'c':'i':'a':'l':
                   '_':'f':'o':'r':'m':'u':'l':'a':'e':'(':'c':'o':'n':'j':'e':
                   'c':'t':'u':'r':'e':'s':',':' ':'E':'M':'L':')':'.':cs)
                         = TokenSemicolon : lexB2Concepts cs
lexB2ContConcepts (c:cs) = lexB2ContConcepts cs

-- Tokenizes a list of concepts.
lexB2Concepts :: String -> [Token]
lexB2Concepts [] = []
lexB2Concepts ('p':'r':'o':'p':'_':'f':'o':'r':'m':'u':'l':'a':cs)
                     = lexB2Concept cs
lexB2Concepts (c:cs) = lexB2Concepts cs

-- Tokenizes a concept.
lexB2Concept :: String -> [Token]
lexB2Concept []                       = [TokenEnd]
lexB2Concept ('b':'o':'x':'(':cs)     = TokenForall : var : TokenOB : lexB2Concept rest
  where (var, rest) = lexVar cs
lexB2Concept ('d':'i':'a':'(':cs)     = TokenExists : var : TokenOB : lexB2Concept rest
  where (var, rest) = lexVar cs
lexB2Concept ('a':'n':'d':cs)         = TokenAnd       : lexB2Concept cs
lexB2Concept ('o':'r':cs)             = TokenOr        : lexB2Concept cs
lexB2Concept ('n':'o':'t':cs)         = TokenNeg       : lexB2Concept cs
lexB2Concept ('t':'r':'u':'e':cs)     = TokenTrue      : lexB2Concept cs
lexB2Concept ('f':'a':'l':'s':'e':cs) = TokenFalse     : lexB2Concept cs
lexB2Concept ('(':cs)                 = TokenOB        : lexB2Concept cs
lexB2Concept (')':cs)                 = TokenCB        : lexB2Concept cs
lexB2Concept (',':cs)                 = lexB2Concept cs
lexB2Concept ('.':cs)                 = lexB2Concept cs
lexB2Concept ('p':'r':'o':'p':'_':'f':'o':'r':'m':'u':'l':'a':cs)
                                      = TokenSemicolon : lexB2Concept cs
lexB2Concept ('e':'n':'d':'_':'o':'f':'_':'l':'i':'s':'t':'.':cs)
                                      = lexB2ContConcepts cs
lexB2Concept (c:cs) 
      | isSpace    c = lexB2Concept cs
      | isAlphaNum c = var : lexB2Concept rest
  where (var, rest) = lexVar (c:cs)
}

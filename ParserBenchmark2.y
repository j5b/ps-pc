{
{- 
   Author: Jannis, Ka Wai
   License: GPL 3.0
   File: ParserBenchmark1.y/hs
   Description: The generated concept parser for benchmarks from
                http://www.cs.man.ac.uk/~schmidt/mspass/problems.html.
-}

module ParserBenchmark2 where

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
      box             { TokenBox }
      dia             { TokenDia }
      and             { TokenAnd }
      or              { TokenOr }
      not             { TokenNeg }
      true            { TokenTrue }
      false           { TokenFalse }
      '('             { TokenOB }
      ')'             { TokenCB }
      ','             { TokenComma }
      formula         { TokenFormula }
      '.'             { TokenFormulaEnd }

%%

File     : begin ConceptSeq end     { $2 }

ConceptSeq
         : formula Concept '.'            { [$2] }
         | ConceptSeq formula Concept '.' { $1 ++ [$3] }

Concept  : and '(' Concept ',' Concept ')' {And $3 $5}
         | or '(' Concept ',' Concept ')'  {Or $3 $5}
         | Concept1                        {$1}

Concept1 : box '(' rel ',' Concept ')' {Forall $3 ($5)}
         | dia '(' rel ',' Concept ')' {Exists $3 ($5)}
         | not Concept2                {Neg $2}
         | Concept2                    {$1}

Concept2 : var                      {Atom $1}
         | true                     {top}
         | false                    {bottom}
         | '(' Concept ')'          {$2}
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenBox
      | TokenDia
      | TokenVar String
      | TokenAnd
      | TokenOr
      | TokenNeg
      | TokenTrue
      | TokenFalse
      | TokenOB
      | TokenCB
      | TokenComma
      | TokenBegin
      | TokenEnd
      | TokenRel String
      | TokenFormula
      | TokenFormulaEnd
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('l':'i':'s':'t':'_':'o':'f':'_':'s':'p':'e':'c':'i':'a':'l':'_':
       'f':'o':'r':'m':'u':'l':'a':'e':'(':'c':'o':'n':'j':'e':'c':
       't':'u':'r':'e':'s':',':' ':'E':'M':'L':')':'.':cs)
             = TokenBegin : lexerConcepts cs
lexer (c:cs) = lexer cs

lexerConcepts :: String -> [Token]
lexerConcepts [] = []
lexerConcepts ('p':'r':'o':'p':'_':'f':'o':'r':'m':'u':'l':'a':cs)
                     = TokenFormula : lexerConcept cs
lexerConcepts ('e':'n':'d':'_':'o':'f':'_':'l':'i':'s':'t':'.':cs)
                     = [TokenEnd]
lexerConcepts (c:cs) = lexerConcepts cs

lexerConcept :: String -> [Token]
lexerConcept ('b':'o':'x':'(':cs)     = TokenBox : TokenOB : lexerRel cs
lexerConcept ('d':'i':'a':'(':cs)     = TokenDia : TokenCB : lexerRel cs
lexerConcept ('a':'n':'d':cs)         = TokenAnd : lexerConcept cs
lexerConcept ('o':'r':cs)             = TokenOr : lexerConcept cs
lexerConcept ('n':'o':'t':cs)         = TokenNeg : lexerConcept cs
lexerConcept ('t':'r':'u':'e':cs)     = TokenTrue : lexerConcept cs
lexerConcept ('f':'a':'l':'s':'e':cs) = TokenFalse : lexerConcept cs
lexerConcept ('(':cs)                 = TokenOB : lexerConcept cs
lexerConcept (')':cs)                 = TokenCB : lexerConcept cs
lexerConcept (',':cs)                 = TokenComma : lexerConcept cs
lexerConcept (c:':':cs)               = TokenFormula : lexerConcept cs
lexerConcept ('.':cs)                 = TokenFormulaEnd : lexerConcepts cs
lexerConcept (c:cs) 
      | isSpace    c = lexerConcept cs
      | isAlphaNum c = lexVar (c:cs)

lexerRel :: String -> [Token]
lexerRel cs =
   case span isAlphaNum cs of
      (rel,rest)   -> TokenRel rel : lexerConcept rest

lexVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexerConcept rest

}

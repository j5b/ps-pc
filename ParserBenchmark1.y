{
{- 
   Author: Jannis, Ka Wai
   License: GPL 3.0
   File: ParserBenchmark1.y/hs
   Description: The generated concept parser for benchmarks from
                http://iamwww.unibe.ch/~lwb/benchmarks/benchmarks.htm.
-}

module ParserBenchmark1 where

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
      box             { TokenBox }
      dia             { TokenDia }
      '&'             { TokenAnd }
      '->'            { TokenImplies }
      '~'             { TokenNeg }
      '('             { TokenOB }
      ')'             { TokenCB }
      'id:'           { TokenFormula }

%%

File     : begin ConceptSeq end     { $2 }

ConceptSeq
         : 'id:' Concept            { [$2] }
         | ConceptSeq 'id:' Concept { $1 ++ [$3] }

Concept  : Concept '->' Concept1    {Or (Neg $1) $3}
         | Concept '&'  Concept1    {And $1 $3}
         | Concept1                 {$1}

Concept1 : box Concept2             {Forall "R" ($2)}
         | dia Concept2             {Exists "R" ($2)}
         | '~' Concept2             {Neg $2}
         | Concept2                 {$1}

Concept2 : var                      {Atom $1}
         | '(' Concept ')'          {$2}
{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenBox
      | TokenDia
      | TokenVar String
      | TokenAnd
      | TokenImplies
      | TokenNeg
      | TokenOB
      | TokenCB
      | TokenBegin
      | TokenEnd
      | TokenFormula
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('b':'e':'g':'i':'n':cs)
             = TokenBegin : lexerConcepts cs
lexer (c:cs) = lexer cs

lexerConcepts :: String -> [Token]
lexerConcepts [] = []
lexerConcepts ('b':'o':'x':cs) = TokenBox : lexerConcepts cs
lexerConcepts ('d':'i':'a':cs) = TokenDia : lexerConcepts cs
lexerConcepts ('&':cs)         = TokenAnd : lexerConcepts cs
lexerConcepts ('-':'>':cs)     = TokenImplies : lexerConcepts cs
lexerConcepts ('~':cs)         = TokenNeg : lexerConcepts cs
lexerConcepts ('(':cs)         = TokenOB : lexerConcepts cs
lexerConcepts (')':cs)         = TokenCB : lexerConcepts cs
lexerConcepts (c:':':cs)       = TokenFormula : lexerConcepts cs
lexerConcepts ('e':'n':'d':cs) = [TokenEnd]
lexerConcepts (c:cs) 
      | isSpace    c = lexerConcepts cs
      | isAlphaNum c = lexVar (c:cs)

lexVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexerConcepts rest

}

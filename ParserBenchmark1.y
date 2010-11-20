{
{- 
   Author: Jannis, Ka Wai
   License: GPL 3.0
   File: Grammar.hs
   Description: The generated concept parser.
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
lexer ('b':'o':'x':cs) = TokenBox : lexer cs
lexer ('d':'i':'a':cs) = TokenDia : lexer cs
lexer ('&':cs)         = TokenAnd : lexer cs
lexer ('-':'>':cs)     = TokenImplies : lexer cs
lexer ('~':cs)         = TokenNeg : lexer cs
lexer ('(':cs)         = TokenOB : lexer cs
lexer (')':cs)         = TokenCB : lexer cs
lexer (c:':':cs)       = TokenFormula : lexer cs
lexer ('b':'e':'g':'i':'n':cs)
                       = TokenBegin : lexer cs
lexer ('e':'n':'d':cs) = TokenEnd : lexer cs
lexer (c:cs) 
      | isSpace    c = lexer cs
      | isAlphaNum c = lexVar (c:cs)

lexVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokenVar var : lexer rest

}

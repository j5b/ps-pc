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

-- concepts: rule: concepts: orbranching : ( : branch 1: ) : ( : branch 2 : ) 
data PTokens = CToken [Concept] | -- CToken: Concepts
               RToken Rule Concept | -- RToken: Rules
               ORToken | -- ORToken : Or branching
               LBToken | -- LBToken : left bracket
               RBToken  -- RBToken : right bracket
    deriving (Eq, Show)

type TokenForm = [PTokens]

validTokenForm :: TokenForm -> Bool
validTokenForm (ORToken : rest) = if rest /= [] then 
               cond1 && cond2 && cond3 && cond4 else
               False
  where cond1 = head rest == LBToken
        cond2 = r /= []
        cond3 = if cond2 then head r == LBToken else False
        cond4 = if cond2 then last r == RBToken else False
        (l,r) = splitAtElem RBToken $ tail rest 
               
splitAtElem e [] = ([],[])
splitAtElem e [x] = if e == x then ([e],[]) else ([],[e])
splitAtElem e (x:xs) 
  | e == x    = ([e],xs)
  | otherwise = (x:l, r)
  where (l,r) = splitAtElem e xs
               

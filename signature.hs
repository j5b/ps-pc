{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: signature.hs
   Description: provides a framework to build and handle description logic concepts
-}

module Signature where

-- TODO: Add forall and or constructor and modify everything below
-- TODO: Change to string

-- Concept is the language definition for description logic ALC and AL
data Concept = T | Atom String | Neg Concept | Or Concept Concept |
     	       And Concept Concept | Exists String Concept | Forall String Concept
     deriving (Show, Eq)

{- 
   WARNING: Eq is dangerous since it only checks if two statements 
   are identical in the restricted language but not equivalence
   it can be very confusing: (assuming Eq we would have)
      - neg top == bottom returns True
      - neg bottom == top returns False
   neither are identical in the extended language
   but sometimes they are in the restricted language    
-}

-- Truth/Tautology
top = T
-- Bottom/Falsity
bottom = Neg T

-- neg negates a concept
neg :: Concept -> Concept
neg concept = Neg concept

-- atom builds and atomic concept
atom :: String -> Concept
atom atomic = Atom atomic 

infixr 8 /\
-- /\ is and/intersection in description logic
(/\) :: Concept -> Concept -> Concept
f1 /\ f2 = And f1 f2

infixr 7 \/
-- \/ is or/union in description logic
(\/) :: Concept -> Concept -> Concept
f1 \/ f2 = Or f1 f2

infixr 6 .>
-- .> is implies/subset
(.>) :: Concept -> Concept -> Concept
f1 .> f2 = (Neg f1) \/ f2

infixr 5 <->
-- <-> is equivalent/same set
(<->) :: Concept -> Concept -> Concept
f1 <-> f2 = (f2 .> f1) /\ (f1 .> f2)

-- exists constructs an exists expression
exists :: String -> Concept -> Concept
exists rel concept = Exists rel concept

-- forall constructs an forall expression
forall :: String -> Concept -> Concept
forall rel concept = Forall rel concept

-- converts a formula to Negation Normal Form
toNNF :: Concept -> Concept
toNNF (Neg (Neg f)) = toNNF f
toNNF (Neg (And f1 f2)) = Or (toNNF $ Neg f1) (toNNF $ Neg f2)
toNNF (Neg (Or f1 f2)) = And (toNNF $ Neg f1) (toNNF $ Neg f2)
toNNF (Neg (Exists str f)) = Forall str $ toNNF (Neg f)
toNNF (Neg (Forall str f)) = Exists str $ toNNF (Neg f)
toNNF concept = concept

-- more clearer name if required
toNegationNormalForm = toNNF


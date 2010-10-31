{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: signature.hs
   Description: provides a framework to build and handle description logic concepts
-}

module Signature where

-- Concept is the language definition for description logic ALC and AL
data Concept atoms rels = T | Atom atoms | Neg (Concept atoms rels) | 
     	       	          And (Concept atoms rels) (Concept atoms rels) | Exists rels (Concept atoms rels)
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
neg :: (Concept a r) -> (Concept a r)
neg concept = Neg concept

-- atom builds and atomic concept
atom :: a -> (Concept a r)
atom atomic = Atom atomic 

infixr 8 /\
-- /\ is and/intersection in description logic
(/\) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 /\ f2 = And f1 f2

infixr 7 \/
-- \/ is or/union in description logic
(\/) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 \/ f2 = Neg ((Neg f1) /\ (Neg f2))

infixr 6 .>
-- .> is implies/subset
(.>) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 .> f2 = (Neg f1) \/ f2

infixr 5 <->
-- <-> is equivalent/same set
(<->) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 <-> f2 = (f2 .> f1) /\ (f1 .> f2)

-- exists constructs an exists expression
exists :: r -> (Concept a r) -> (Concept a r)
exists rel concept = Exists rel concept

-- forall constructs an forall expression
forall :: r -> (Concept a r) -> (Concept a r)
forall rel concept = Neg (Exists rel $ Neg concept)


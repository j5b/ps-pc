-- file: signature.hs

module Signature where

-- import HUnit

-- type rel denotes in the signature i.e. all relations allowed
-- How do we allow for empty signature i.e. with no relations?
-- e.g. rels could be data rels = HasChild Int Int | IsParent Int Int
-- Should we create a Relation class?
-- Atom Integer was simply WRONG couldn't express simple C
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

top = T
bottom = Neg T

-- Still doesn't satify binding rule
neg :: (Concept a r) -> (Concept a r)
neg concept = Neg concept

atom :: a -> (Concept a r)
atom atomic = Atom atomic 

infixr 8 /\
(/\) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 /\ f2 = And f1 f2

infixr 7 \/
(\/) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 \/ f2 = Neg ((Neg f1) /\ (Neg f2))

infixr 6 .>
(.>) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 .> f2 = (Neg f1) \/ f2

infixr 5 <->
(<->) :: (Concept a r) -> (Concept a r) -> (Concept a r)
f1 <-> f2 = (f2 .> f1) /\ (f1 .> f2)

exists :: r -> (Concept a r) -> (Concept a r)
exists rel concept = Exists rel concept

forall :: r -> (Concept a r) -> (Concept a r)
forall rel concept = Neg (Exists rel $ Neg concept)


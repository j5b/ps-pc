-- file: signature.hs

module Signature where

-- type t denotes the signature i.e. all relations allowed
-- How do we allow for empty signature i.e. with no relations
-- data Modal = Exists | ForAll
-- e.g. t could be data t = HasChild Int Int | IsParent Int Int
data Concept t = T | Atom Int | Neg (Concept t) | 
     	       	 And (Concept t) (Concept t) | Exists t (Concept t)
     deriving (Eq, Show)

top = T
bottom = Neg T

(&) :: (Concept t) -> (Concept t) -> (Concept t)
f1 & f2 = And f1 f2

-- this one is NOT IDEAL TODO: Find a symbol
(!) :: (Concept t) -> (Concept t) -> (Concept t)
f1 ! f2 = Neg ((Neg f1) & (Neg f2))

not :: (Concept t) -> (Concept t)
not concept = Neg concept

-- find something better than Eq t
exists :: Eq t => t -> (Concept t) -> (Concept t)
exists rel concept = Exists rel concept

forall :: Eq t => t -> (Concept t) -> (Concept t)
forall rel concept = Neg (Exists rel $ Neg concept)

(.>) :: (Concept t) -> (Concept t) -> (Concept t)
f1 .> f2 = (Neg f1) ! f2

(=:=) :: (Concept t) -> (Concept t) -> (Concept t)
f1 =:= f2 = (f2 .> f1) & (f1 .> f2)

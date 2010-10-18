-- file: signature.hs

module Signature where

-- type t denotes the signature i.e. all relations allowed
-- How do we allow for empty signature i.e. with no relations
-- data Modal = Exists | ForAll
-- e.g. t could be data t = HasChild Int Int | IsParent Int Int
data Formula t = T | Atom Int | Neg (Formula t) | 
     	       	 And (Formula t) (Formula t) | Exists t (Formula t)
     deriving (Eq, Show)

top = T
bottom = Neg T

(&) :: (Formula t) -> (Formula t) -> (Formula t)
f1 & f2 = And f1 f2

(!):: (Formula t) -> (Formula t) -> (Formula t)
f1 ! f2 = Neg ((Neg f1) & (Neg f2))

-- need a way to define Forall
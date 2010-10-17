-- file: signature.hs

module Signature where

-- type t denotes the signature i.e. all relations allowed
-- How do we allow for empty signature i.e. with no relations
data Modal = Exists | ForAll
data Formula t = F | T | Atom Int |
                 Neg (Formula t) | And (Formula t) (Formula t) |
                 Or (Formula t) (Formula t) | Modal t (Formula t)
     deriving (Eq, Show)

 



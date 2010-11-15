{- 
   Author: <authors name>
   Maintainer: <maintainer's name>
   Email: <maintainer's email>
   License: GPL 3.0
   File: filename
   Description: short description
-}

{-
   General comments
-}

{-
   Always explain functions just above them so Hackage can help us generate documentation
   "TODO:" Will denote a task to do
   "WARNING:" Some important information to put in the documentation 
-}

-- simple concepts to test sc for simple concept
sc1 = top -- always true
sc2 = bottom -- always false
sc3 = atom "C" 
sc4 = atom "D"
sc5 = neg sc3
sc6 = neg sc4
sc7 = sc3 \/ sc4
sc8 = sc3 /\ sc4
sc9 = sc3 \/ (neg sc4)
sc10 = sc3 /\ (neg sc4)
sc11 = sc3 /\ top -- should be the same as sc3
sc12 = sc3 /\ bottom -- alawys false
sc13 = sc3 \/ top -- always true
sc14 = sc3 \/ bottom -- should be the same as sc3
sc15 = forall "R" top -- should always be true
sc16 = forall "R" bottom -- should only be true if there are no successors
sc17 = forall "R" sc3
sc18 = forall "R" sc4
sc19 = forall "R" (neg sc3)
sc20 = forall "S" sc3
sc21 = forall "S" (sc3 /\ sc4)
sc22 = forall "S" (sc3 \/ sc4)
sc23 = forall "S" (sc3 /\ bottom) -- should be only true if there are no successors
sc24 = exists "R" top -- should always be true unless there are no successors
sc25 = exists "R" bottom -- should always be false
sc26 = exists "R" sc3
sc27 = exists "R" sc4
sc28 = exists "S" (neg sc4)
sc29 = exists "S" (sc3 /\ sc4)
sc30 = exists "S" (sc3 \/ sc4)
sc31 = exists "S" (sc3 /\ bottom) -- should be always false

-- nasty cases
nt1 = top \/ bottom
nt2 = top /\ bottom
nt3 = exists "R" bottom
nt4 = exists "R" top /\ forall "R" bottom
{- 
   Author:
   License: GPL 3.0
   Description: Some common test utilities
-}

module TestUtils where

import Signature
import Model

import Data.List
import Test.HUnit

-- testing function YOU SHOULD USE TO MAKE YOUR FILE EASIER

printCG :: [Concept] -> [Concept] -> String
printCG concepts gamma = "\n\tGamma: "++show gamma++"\n\tConcepts: "++show concepts

testequality msg target result inputString = TestCase (assertEqual newMsg target result)
  where newMsg = "\nERROR DETECTED: "++msg++"\nFor input:"++inputString++"\n"

maplabel label testlist = TestList $ map labelMaker $ zip [1..] testlist
  where labelMaker (a,b) = TestLabel (label++" "++show a) b

---- our concepts we will use

-- basics + top and bottom

atoma = atom "A"
atomb = atom "B"
atomc = atom "C"
atomd = atom "D"
atome = atom "E"
atomf = atom "F"
notatoma = neg atoma
notatomb = neg atomb
notatomc = neg atomc
notatomd = neg atomd
notatome = neg atome
notatomf = neg atomf

-- disjunctions

top_or_bottom = top \/ bottom
a_or_b = atoma \/ atomb
c_or_d = atomc \/ atomd
c_or_nd = atomc \/ notatomd
c_or_top = atomc \/ top
c_or_bottom = atomc \/ bottom

-- conjunctions

top_and_bottom = top /\ bottom
a_and_b = atoma /\ atomb
c_and_d = atomc /\ atomd
c_and_nd = atomc /\ notatomd
c_and_top = atomc /\ top
c_and_bottom = atomc /\ bottom

-- universals simple

forall_r_top = forall_r top
forall_r_bottom = forall_r bottom
forall_r_a = forall_r atoma
forall_r_b = forall_r atomb
forall_r_c = forall_r atomc
forall_r_d = forall_r atomd 
forall_r_nc = forall_r notatomc
forall_s_c = forall_s atomc

-- existential simple

exists_r_top = exists_r top
exists_r_bottom = exists_r bottom
exists_r_a = exists_r atoma
exists_r_b = exists_r atomb
exists_r_c = exists_r atomc
exists_r_d = exists_r atomd
exists_s_nd = exists_s notatomd

-- complicated for sagie's part

andExists = exists_r_a /\ exists_r_b
andforalls = forall_r_a /\ forall_r_b
orExists = exists_r_a \/ exists_r_b
orforalls = forall_r_a \/ forall_r_b
negExists = neg exists_r_a
negForalls = neg forall_r_a
existsexists = exists_s exists_r_a
existsforalls = exists_s forall_r_a
forallexists = forall_s  exists_r_a

-- THIS WILL GENERATE LOTS AND LOTS OF CONCEPTS

forall_r = forall "R"
forall_s = forall "S"
exists_r = exists "R"
exists_s = exists "S"

simple_atom_list :: Int -> [Concept]
simple_atom_list num 
  | num > 26  = error "Not enough characters"
  | otherwise = atoms ++ negs
  where chars = take num "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        atoms = map (Atom . (:[])) chars
        negs  = map Neg atoms

all_tuples [] list = []
all_tuples (elem:rest) list =
  map (tupler elem) list++all_tuples rest list
    where tupler elem another = (elem,another)

or_generator = map or_maker
  where or_maker (a,b) = a \/ b 

and_generator = map and_maker
  where and_maker (a,b) = a /\ b

forall_generator list = map forall_r list ++ map forall_s list

exists_generator list = map exists_r list ++ map exists_r list

generateConcepts num  = simple ++ foralls ++ exists ++ complex
  where simple_atoms  = simple_atom_list num
        simple        = simple_atoms ++ simpleOr ++ simpleAnd
        simpleOr      = or_generator simpleTuples
        simpleAnd     = and_generator simpleTuples
        foralls       = forall_generator $ simple ++ [top,bottom]
        exists        = exists_generator $ simple ++ [top,bottom]
        simpleTuples  = all_tuples simple_atoms simple_atoms
        complex       = forall_generator (foralls++exists) ++
                        exists_generator (foralls++exists)

containsExists (Exists str concept) = True
containsExists (Forall str concept) = containsExists concept
containsExists _ = False

-- simple concepts to test sc for simple concept
sc28 = exists "S" notatomd
sc29 = exists "S" (atomc /\ atomd)
sc30 = exists "S" (atomc \/ atomd)
sc31 = exists "S" (atomc /\ bottom) -- should be always false

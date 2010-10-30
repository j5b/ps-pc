-- file: signature_test.hs
import Signature

-- EXAMPLES
data ExampleAtoms = C | D | E deriving (Show, Eq)
data ExampleRelations = R | S deriving (Show, Eq)
type Ea = ExampleAtoms
type Er = ExampleRelations

-- simple checks constructors
simple1, simple2, simple3, simple4, simple5, simple6, simple7, simple8 :: (Concept Ea Er)
simple1 = atom C
simple2 = top \/ bottom
simple3 = top /\ simple1
simple4 = neg simple1
simple5 = simple1 .> simple4
simple6 = simple1 <-> (neg simple4) -- this one looks very complicated
simple7 = exists R simple1
simple8 = forall S bottom

-- binding tests
bindTest1 = part1 == part2
  where part1, part2 :: (Concept Ea Er)
        part1 = top \/ bottom /\ top
        part2 = top \/ (bottom /\ top)
bindTest2 = part1 == part2
   where part1, part2 :: (Concept Ea Er)
         part1 = neg top /\ bottom 
         part2 = neg (top /\ bottom)
bindTest3 = part1 == part2
   where part1, part2 :: (Concept Ea Er)
         part1 = (atom C) \/ (atom D) .> top
         part2 = ((atom C) \/ (atom D)) .> top
bindTest4 = part1 == part2 
   where part1, part2 :: (Concept Ea Er)
         part1 = top /\ bottom <-> bottom \/ top
         part2 = (top /\ bottom) <-> (bottom \/ top) 

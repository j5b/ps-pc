{- 
   Author: Ka Wai
   License: GPL 3.0
   File: OutputModel_test.hs
   Description: tests for outputs generated by OutputModel.hs
-}

module OutputModel_test where

import System.IO
import OutputModel
import Signature
import TestUtils
import Test.HUnit

domainTests = maplabel "Output model domain test"
  [domaintest1, domaintest2, domaintest3, domaintest4]

nodesizeTests = maplabel "Output model nodesize test"
  [nodesizetest1, nodesizetest2, nodesizetest3, nodesizetest4]

errorTests = maplabel "Output model error test"
  [errortest1, errortest2, errortest3, errortest4]

alltests = do putStrLn "==== Testing the model output"
              runTestTT domainTests
              runTestTT nodesizeTests
              runTestTT errorTests


-- Testing setup

dom0 = []
dom1 = [1]
dom2 = [1,2,3]
dom3 = [1,2,3,4]
dom4 = [1,2,3,4,5,6]
domall = dom1 ++ dom2

unary0 = []
unary1 = [("A",[])]
unary2 = [("B",[1])]
unary3 = [("C",[1]), ("D",[2,3])]
unary4 = [("E",[1,3]), ("A",[2,4,1])]
unaryall = concat [unary2, unary3, unary4]

binary0 = []
binary1 = [("R",[(1,1)])]
binary2 = [("R",[(1,2),(1,2)])]
binary3 = [("R",[(1,2),(2,3)])]
binary4 = [("A",[(4,1)]),("R",[(4,1)])]
binaryall = binary1 ++ binary4

-- Expected results

emptydom = begin ++ "label = \"Domain is empty, no model to draw\" ;\n" ++ end
dupdomain = begin ++ "label = \"Domain contains duplicated individuals\" ;\n" ++ end
dupunary = begin ++ "label = \"Duplicated unary relation names exist\" ;\n" ++ end
dupbinary = begin ++ "label = \"Duplicated binary relation names exist\" ;\n" ++ end

begin = "digraph {\n "
end   = "}"

-- Tests
domaintest1 = testequality msg target result "([1],[(A,[])], [(R,[(1,1)])])"
  where msg    = "Failed to produce correct output for simple model"
        result = modelToGraph (dom1, unary1, binary1)
        target = begin ++ "1 [label=\"1\"] ;\n 1 -> 1 [label=\"R\"] ;\n " ++ end

domaintest2 = testequality msg target result
              "([1,2,3],[(B,[1])], [(R,[(1,2),(2,1)])])"
  where msg    = "Failed to produce correct output for simple model"
        result = modelToGraph (dom2, unary2, binary2)
        target = begin ++ "1 [label=\"1: B\"] ;\n 2 [label=\"2\"] ;\n 3 " ++
                 "[label=\"3\"] ;\n 1 -> 2 [label=\"R\"] ;\n " ++ end

domaintest3 = testequality msg target result
              "([1,2,3,4],[(C,[1]), (D,[2,3])], [(R,[(1,2),(2,3)])])"
  where msg    = "Failed to produce correct output for simple model"
        result = modelToGraph (dom3, unary3, binary3)
        target = begin ++ "1 [label=\"1: C\"] ;\n 2 [label=\"2: D\"] ;\n " ++
                 "3 [label=\"3: D\"] ;\n 4 [label=\"4\"] ;\n 1 -> 2 " ++
                 "[label=\"R\"] ;\n 2 -> 3 [label=\"R\"] ;\n " ++ end

domaintest4 = testequality msg target result "([1,1,2,3,1,2,3,4,5,6],[(E,[1,3]), (A,[2,4,1])], [(A,[(4,1)]),(R,[(4,1)])])"
  where msg    = "Failed to produce correct output for simple model"
        result = modelToGraph (dom4, unary4, binary4)
        target = begin ++ "1 [label=\"1: A, E\"] ;\n 2 [label=\"2: A\"] ;\n" ++
                 " 3 [label=\"3: E\"] ;\n 4 [label=\"4: A\"] ;\n 5 " ++
                 "[label=\"5\"] ;\n 6 [label=\"6\"] ;\n 4 -> 1 " ++
                 "[label=\"A\"] ;\n 4 -> 1 [label=\"R\"] ;\n " ++ end

nodesizetest1 = testequality msg target result
  "([1,2,3,4,5,6],[(B,[1]),(C,[1]),(D,[2,3]),(E,[1,3]),(A,[2,4,1])], [(A,[(4,1)]),(R,[(4,1)])]))"
  where msg    = "Failed to produce correct output for many unary relations"
        result = modelToGraph (dom4, unaryall, binary4)
        target = begin ++ "1 [label=\"1: A, E, C, \\nB\"] ;\n 2 [label=\"2: " ++
                 "A, D\"] ;\n 3 [label=\"3: E, D\"] ;\n 4 [label=\"4: A\"]" ++
                 " ;\n 5 [label=\"5\"] ;\n 6 [label=\"6\"] ;\n 4 -> 1 " ++
                 "[label=\"A\"] ;\n 4 -> 1 [label=\"R\"] ;\n " ++ end

-- Unary name of just right length (8 characters)
nodesizetest2 = testequality msg target result "([1],[(justsize,[1])],[])"
  where msg    = "Failed to produce correct output for short length unary name"
        result = modelToGraph ([1], [("justsize",[1])], [])
        target = begin ++ "1 [label=\"1: justsize\"] ;\n " ++ end

-- Unary name of just over length (9 characters)
nodesizetest3 = testequality msg target result "([1],[(istoolong,[1])],[])"
  where msg    = "Failed to produce correct output for simple model"
        result = modelToGraph ([1], [("istoolong",[1])], [])
        target = begin ++ "1 [label=\"1: istoolo-\\nng\"] ;\n " ++ end

nodesizetest4 = testequality msg target result "([1],[(A,[1]),(justsize,[1]),(B,[1]),(C,[1]),(istoolong,[1]),(D,[1]),(E,[1])],[])"
  where msg    = "Failed to produce correct output for mixture of unary name lengths"
        result = modelToGraph ([1], [("A",[1]),("justsize",[1]),("B",[1]),
                 ("C",[1]),("istoolong",[1]),("D",[1]),("E",[1])], [])
        target = begin ++ "1 [label=\"1: E, D, \\nistoolo-\\nng, C, B, \\n" ++
                 "justsize, \\nA\"] ;\n " ++ end

errortest1 = testequality msg target result "empty model ([],[],[])"
  where msg    = "Failed to detect empty domain"
        result = modelToGraph (dom0, unary0, binary0)
        target = emptydom

errortest2 = testequality msg target result "duplicates in domain ([1,1,2,3],[],[])"
  where msg    = "Failed to detect duplicated individuals in domain"
        result = modelToGraph (domall, unary0, binary0)
        target = dupdomain

errortest3 = testequality msg target result "duplicates in domain ([1,2,3,4],[(A,[]),(E,[1,3]),(A,[2,4,1])],[])"
  where msg    = "Failed to detect duplicated unary concepts in unarys"
        result = modelToGraph (dom3, unary1 ++ unary4, binary0)
        target = dupunary

errortest4 = testequality msg target result "duplicates in domain ([1,2,3,4],[],[(R,[(1,1)]),(A,[(4,1)]),(R,[(4,1)])])"
  where msg    = "Failed to detect duplicated unary concepts in binarys"
        result = modelToGraph (dom3, unary0, binaryall)
        target = dupbinary
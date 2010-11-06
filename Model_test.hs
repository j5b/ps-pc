{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model_test.hs
   Description: test models
-}

import Model
import Report
import Test.HUnit

-- TODO: Add formal test cases

assertSuccess (log,success) = assertBool success

onePointDomain = [1]
onePointSimpleModel = (onePointDomain, [], [])

oneAtomModel = [("A", [1])]
oneRelationModel = [("R", [(1,1)])]

simpleModel1 = (onePointDomain, oneAtomModel, [])
simpleModel2 = (onePointDomain, oneAtomModel, oneRelationModel)

nonTrivialDomain_1 = [1,2,3]
nonTrivialAtoms_1 = [("A", [1,2,3]),
                   ("B", [1])]
nonTrivialRelations_1 = [("R", [(1,2),(1,3)]),
                         ("S", [(1,1),(2,2),(3,3)])]

simpleModel3 = (nonTrivialDomain_1, nonTrivialAtoms_1, nonTrivialRelations_1)

errorDomain_1 = [1]
errorAtoms_1 = [("A", [2])]
errorRelations_1 = [("R", [(2,1)])]

errorModel1 = (errorDomain_1, errorAtoms_1, errorRelations_1)


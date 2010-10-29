-- file: model.hs

module Model where

import Control.Monad

type Individual = Integer
type Domain = [Individual]
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])
type Model = (Domain, [UnaryRelation], [BinaryRelation])

-- the following two function return Nothing if the relation name is not part of the model
-- otherwise it return true or false according to whether R(a) or R(a,b) is true in the
-- model or not

inModel :: Individual -> Domain -> Bool
-- TODO: Simplify this
inModel ind dom = elem ind dom

-- code compiles but should be updated
-- TODO: Fix this so no Nothings
isInModelBin :: String -> (Individual,Individual) -> Model -> Maybe Bool
isInModelBin name (a,b) (dom, _, binaryModel)
  | (inModel a dom) & (inModel b dom) = Nothing
  | otherwise = liftM (elem (a,b)) $ lookup name binaryModel

--code compiles but should be updated
-- TODO: Fix this so no Nothings
isInModelUn :: String -> Individual -> Model -> Maybe Bool
isInModelUn name a (dom, unaryModel, _)
  | inModel a dom = Nothing
  | otherwise = liftM (elem a) $ lookup name unaryModel


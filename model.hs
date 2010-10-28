-- file: model.hs

module Model where

import Control.Monad

type Individual = Integer
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])
type Model = ([UnaryRelation], [BinaryRelation])

-- the following two function return Nothing if the relation name is not part of the model
-- otherwise it return true or false according to whether R(a) or R(a,b) is true in the
-- model or not

isInModelBin :: String -> (Individual,Individual) -> Model -> Maybe Bool
isInModelBin name (a,b) (_, binaryModel)
  = liftM (elem (a,b)) $ lookup name binaryModel

isInModelUn :: String -> Individual -> Model -> Maybe Bool
isInModelUn name a (unaryModel, _)
  = liftM (elem a) $ lookup name unaryModel


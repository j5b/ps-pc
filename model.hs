{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: provides a framework to build and handle description logic models
-}

module Model where

import Data.Maybe

-- A model is a set of individual for which atomic or binary relations may or may not hold
type Individual = Integer
type Domain = [Individual]
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])
type Model = (Domain, [UnaryRelation], [BinaryRelation])

-- the following functions check if the individual is part of the domain
inModel :: Individual -> Domain -> Bool
inModel = elem

-- the following function checks if name(a,b) holds in the given model
isInModelBin :: String -> (Individual,Individual) -> Model -> Bool
isInModelBin name (a,b) (dom, _, binaryModel)
  | not $ (inModel a dom) && (inModel b dom) = error errorMessage
  | otherwise                                = elem (a,b) $ tryLookup name binaryModel
  where errorMessage = "Individuals "++(show a)++" and "++(show b)++" not in domain"

-- the following function checks if name(a) holds in the given model
isInModelUn :: String -> Individual -> Model -> Bool
isInModelUn name a (dom, unaryModel, _)
  | not $ inModel a dom = error errorMessage
  | otherwise           = elem a $ tryLookup name unaryModel
  where errorMessage = "Individual "++ (show a) ++ " not in domain"

-- This function search for a in a list [(a,b)] and returns b if possible
-- otherwise it displays a meaningful message
tryLookup :: String -> [(String,a)] -> a
tryLookup name domain
  | isJust $ result = fromJust result
  | otherwise       = error ("Couldn't find relation/atom"++name++" inside the domain")
  where result = lookup name domain



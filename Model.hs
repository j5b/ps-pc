{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: provides a framework to build and handle description logic models
-}

module Model (Model, Individual, Domain, BinaryRelation, UnaryRelation,
              getDomain, getRelations, isEmpty,
              emptyModel, checkModelConsistency, isInUnary, isInBinary) where

import Control.Monad
import Data.Maybe
import Report

-- A model is a set of individual for which atomic or binary relations may or may not hold
type Individual = Integer
type Domain = [Individual]
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])
type Model = (Domain, [UnaryRelation], [BinaryRelation])

-- check if the model is empty
isEmpty model = flip (==) [] $ getDomain model

-- returns the domain of the model
getDomain (dom, _, _) = dom

-- returns the relations of the model
getRelations (_, _, relations) = relations

isInPrototype element index mapper = fromMaybe False $ liftM (elem element) $ lookup index mapper

-- Checks if atomic concept is in model for given individual
isInUnary :: Individual -> String -> Model -> Bool
isInUnary ind str (_,uns,_) = isInPrototype ind str uns

-- Checks if relation is in model for given tuple of individuals
isInBinary :: (Individual, Individual) -> String -> Model -> Bool
isInBinary pair str (_,_,bins) = isInPrototype pair str bins

-- Creates an empty model (good for testing)
emptyModel = ([], [], [])

-- report for the empty model
emptyModelReport = ("", ["Model is fine since we got an empty result"], True)
emr = emptyModelReport

-- checks the domain for errors
checkModelConsistency :: Model -> Report
checkModelConsistency (dom, uns, bins) = 
  combine reportUns reportBins
  where reportUns  = pushTitle $ title "Unary check" $ checkUnaryConsistency dom uns
        reportBins = pushTitle $ title "Binary check" $ checkBinaryConsistency dom bins
  
-- checks the domain for errors relating to atoms
checkUnaryConsistency :: Domain -> [UnaryRelation] -> Report
checkUnaryConsistency dom list = foldl1 combine $ makeCorrect (map checker list) [] [emr] -- [emr] since a list is expceted
  where checker (name, set) = flip buildReport result $ 
                                   "Unary " ++ name ++ 
                                    (if result then " is only containing elements of the domain" 
                                               else " is not only containing elements of the domain")  
          where result      = allElements set dom

-- checks the domain for errors relating to binary relations
checkBinaryConsistency :: Domain -> [BinaryRelation] -> Report
checkBinaryConsistency dom list = foldl1 combine $ makeCorrect (map checker list) [] [emr] -- [emr] since a list is expected
  where checker (name, set) = flip buildReport result $ 
                                   "Binary " ++ name ++ 
                                    (if result then " is only containing elements of the domain" 
                                               else " is not only containing elements of the domain")  
          where result      = allElements (map fst set) dom && allElements (map snd set) dom

allElements subList list = and $ map (flip elem list) subList

makeCorrect result exception alternative =
  if result == exception then alternative else result




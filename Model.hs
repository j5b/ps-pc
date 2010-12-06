{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: model.hs
   Description: provides a framework to build and handle description logic models
-}

module Model (Model, Individual, Domain, BinaryRelation, UnaryRelation,
              getDomain, getRelations, isEmpty, sortModel, 
              emptyModel, isInUnary, isInBinary) where

import Control.Monad
import Data.Maybe
import Data.List

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

getUnarys (_,unarys,_) = unarys

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

joinTuples :: [(a,a)] -> [a]
joinTuples list = left ++ right
  where (left,right) = unzip list

-- Checking for model consistency
consistentModel :: Model -> String
-- additional cases can be added easily
consistentModel model = if isEmpty model then mzero else concat [case1, case2, case3] 
  where domain  = getDomain model
        unarys  = concatMap snd $ getUnarys model
        binarys = joinTuples $ concatMap snd $ getRelations model
        case1   = if nub unarys \\ domain /= []
                  then "Some atomic concept refers to an individual outside the domain\n" 
                  else mzero
        case2   = if nub binarys \\ domain /= []
                  then "Some binary concept refers to an individual outside the domain\n"
                  else mzero
        case3   = if domain /= nub domain 
                  then "Domain contains duplicates"
                  else mzero

mapSnd :: (a->b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y)
                      
sortModel :: Model -> Model
sortModel model = if result == mzero then sortedModel else error result
  where result = consistentModel model
        sortedModel = (domsorted, unarysorted, binsorted)
        domsorted   = sort $ getDomain model
        unarysorted = sort $ map (mapSnd sort) $ getUnarys model
        binsorted   = sort $ map (mapSnd sort) $ getRelations model

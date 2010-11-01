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
import Report

-- A model is a set of individual for which atomic or binary relations may or may not hold
type Individual = Integer
type Domain = [Individual]
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])
type Model = (Domain, [UnaryRelation], [BinaryRelation])

-- Creates an empty model (good for testing)
emptyModel = ([], [], [])

-- report if the domain is empty
emptyModelReport = ("Model is fine since we got an empty result", True)
emr = emptyModelReport

-- checks the domain for errors
checkModel :: Model -> Report
checkModel (dom, uns, bins) = 
  combine reportUns reportBins
  where reportUns  = title "=> Unary check" $ checkUnary dom uns
        reportBins = title "=> Binary check" $ checkBinary dom bins
  
-- checks the domain for errors relating to atoms
checkUnary :: Domain -> [UnaryRelation] -> Report
checkUnary dom list = foldl1 combine $ makeCorrect (map checker list) [] [emr] -- [emr] since a list is expceted
  where checker (name, set) = flip buildReport result $ 
                                   "Unary " ++ name ++ 
                                    (if result then " is only containing elements of the domain" 
                                               else " is not only containing elements of the domain")  
          where result      = allElements set dom

-- checks the domain for errors relating to binary relations
checkBinary :: Domain -> [BinaryRelation] -> Report
checkBinary dom list = foldl1 combine $ makeCorrect (map checker list) [] [emr] -- [emr] since a list is expected
  where checker (name, set) = flip buildReport result $ 
                                   "Binary " ++ name ++ 
                                    (if result then " is only containing elements of the domain" 
                                               else " is not only containing elements of the domain")  
          where result      = allElements (map fst set) dom && allElements (map snd set) dom

allElements subList list = and $ map (flip elem list) subList

makeCorrect result exception alternative =
  if result == exception then alternative else result




-- file: model.hs

module Model where

type Individual = Integer
type BinaryRelation = (String, [(Individual,Individual)])
type UnaryRelation = (String, [Individual])

-- notice how easy it is to generalise
isInModelBin :: String -> (Individual,Individual) -> BinaryRelation -> Bool
isInModelBin relation (a,b) modelBinary = 
             if fst modelBinary == relation
             then elem (a,b) $ snd modelBinary
             else False

isInModelUn :: String -> Individual -> UnaryRelation -> Bool
isInModelUn relation a modelUnary =
            if fst modelBinary == relation
            then elem a $ snd modelBinary
            else False
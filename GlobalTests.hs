{- 
   Author: Michal Parusinski
   Maintainer: Michal Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: filename
   Description: I will testing the whole lot of findPOM and the proof and model checker
-}

module GlobalTests where

import Test.HUnit
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.List
import System.Random

import Signature
import ProofSearch
import TestUtils
import ModelChecker
import ProofChecker
import Model
import Proof

untilTimeout = 180 -- 30

-- run two threads and return the first one to finish
compete :: IO a -> IO a -> IO a 
compete first second = do mvar <- newEmptyMVar
                          tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) [first,second]
                          result <- takeMVar mvar
                          mapM_ killThread tids
                          return result

-- returns the value of a function unless it timeouts
timeout :: Int -> IO a -> IO (Maybe a) 
timeout sec action = compete (fmap Just action) (threadDelay (sec*1000000) >> return Nothing)

-- return the result without taking care for timeouts
templateSimple :: [Concept] -> [Concept] -> IO (Maybe String)
templateSimple concepts gamma  = do checkResult <- (check concepts gamma searchResult)
                                    if snd checkResult 
                                       then return Nothing 
                                       else return . Just $ errorMsg++fst checkResult
    where searchResult         = findPOM (map toNNF concepts) (map toNNF gamma)
          errorMsg             = "For gamma: "++show gamma++"\n"++
                                 "For concepts: "++show concepts++"\n"++
                                 "A failure was detected:\n"

-- return the result with taking care for timeouts
template :: [Concept] -> [Concept] -> Assertion
template concepts gamma = do result <- timeout untilTimeout (templateSimple concepts gamma)
                             if isNothing result
                                 then assertFailure timeoutMsg
                                 else let actualResult = fromJust result
                                      in unless (isNothing actualResult) $ assertFailure $ fromJust actualResult
                                 where timeoutMsg = "For gamma: "++show gamma++"\n"++
                                                    "For concepts: "++show concepts++"\n"++
                                                    "Detected potential infinite loop"
                         
testtemplate cs gamma = TestCase (template cs gamma)

-- check if the result of computation is correct or not
check :: [Concept] -> [Concept] -> Either Model ProofTree -> IO (String, Bool)
check cs gamma (Left model) = do putStrLn $ " #### MODEL: "++show cs++" "++show gamma
                                 return $ checkInputModel model gamma cs
check _ gamma (Right proof) = do putStrLn $ " #### PROOF: "++show gamma
                                 return $ checkProof proof (map toNNF gamma)

-- Extract the element of the list at the positions given by indicies
-- the elements are given in order for more performance (n+klogk if better than nk)
-- e.g. extract [3,5,4] "abcdefghi" = "def"
extract :: [Int] -> [a] -> [a]
extract indices list 
    | tooLargeIndices = error "Some of the indices provided were too large"
    | otherwise       = extract' 0 (sort $ nub indices) list
    where maxIndex        = length list
          tooLargeIndices = any (>=maxIndex) indices
          extract' :: Int -> [Int] -> [a] -> [a]
          extract' _ [] _ = []
          extract' n (i:is) (x:xs) 
              | n == i = x : extract' (n+1) is xs
              | otherwise =  extract' (n+1) (i:is) xs

-- generate a list of num indices with do not exceed max
generateIndices :: StdGen -> Int -> Int -> [Int]
-- PRE: num >= 0
generateIndices _ 0 _ = []
generateIndices randomGen num max = mod randomint max : generateIndices nextgen (num-1) max
  where (randomint, nextgen) = next randomGen 

generateTest :: StdGen -> Int -> Int -> Test
generateTest randomGen num sampleSize = testtemplate givens gamma
  where (seed1, gen2) = next randomGen
        (seed2, _ )   = next gen2
        indicesGamma  = generateIndices (mkStdGen seed1) sampleSize maxlength
        indicesGivens = generateIndices (mkStdGen seed2) sampleSize maxlength
        concepts      = generateConcepts num
        gamma         = extract indicesGamma concepts
        givens        = extract indicesGivens concepts
        maxlength     = length concepts

globaltests = maplabel "GlobalTests" testlist
  where testlist = take numTests $ map generator [1..]
        numTests = 40
        sampleSize = 6
        simpleSize = 6
        generator x = generateTest (mkStdGen x) simpleSize sampleSize

allglobaltests  = do putStrLn "==== Global tests"
                     runTestTT globaltests

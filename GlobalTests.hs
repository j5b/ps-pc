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

import Signature
import ProofSearch
import TestUtils
import ModelChecker
import ProofChecker
import Model
import Proof

untilTimeout = 30

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
templateSimple concepts gamma  =
    if snd checkResult then return Nothing else return $ Just errorMsg
    where searchResult         = findPOM concepts gamma
          checkResult          = check concepts gamma searchResult
          errorMsg             = "For gamma: "++show gamma++"\n"++
                                 "For concepts: "++show concepts++"\n"++
                                 "A failure was detected:\n\t"++fst checkResult

-- return the result with taking care for timeouts
template :: [Concept] -> [Concept] -> Assertion
template concepts gamma = do result <- timeout untilTimeout (templateSimple concepts gamma)
                             if isNothing result
                                 then assertFailure timeoutMsg
                                 else let actualResult = fromJust result
                                      in if isNothing actualResult
                                         then return ()
                                         else assertFailure $ fromJust actualResult
                                 where timeoutMsg = "For gamma: "++show gamma++"\n"++
                                                    "For concepts: "++show concepts++"\n"++
                                                    "Detected potential infinite loop"
                         
testtemplate cs gamma = TestCase (template cs gamma)

-- check if the result of computation is correct or not
check :: [Concept] -> [Concept] -> Either Model ProofTree -> (String, Bool)
check cs gamma (Left model) = checkInputModel model gamma cs
check _ gamma (Right proof) = checkProof proof gamma

globaltest1 = testtemplate [] [atoma]
globaltest2 = testtemplate [atoma] []
globaltest3 = testtemplate [atoma] [atoma]
globaltest4 = testtemplate [] [forall_r_top]
globaltest5 = testtemplate [forall_r_top] []
globaltest6 = testtemplate [forall_r_top] [forall_r_top]
globaltest7 = testtemplate [] [exists_r_bottom]
globaltest8 = testtemplate [exists_r_bottom] []
globaltest9 = testtemplate [exists_r_bottom] [exists_r_bottom]
globaltest10 = testtemplate [] [a_or_b]
globaltest11 = testtemplate [a_or_b] []
globaltest12 = testtemplate [a_or_b] [a_or_b]

globaltests = maplabel "GlobalTests"  [globaltest1, globaltest2, globaltest3,
                        globaltest4, globaltest5, globaltest6, 
                        globaltest7, globaltest8, globaltest9,
                        globaltest10, globaltest11, globaltest12]

allglobaltests  = do putStrLn "==== Global tests"
                     runTestTT globaltests
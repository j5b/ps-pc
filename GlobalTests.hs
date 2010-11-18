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

import Main
import Signature
import ProofSearch
import TestUtils

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

globaltest1 = testtemplate [] [nt1]
globaltest2 = testtemplate [nt1] []
globaltest3 = testtemplate [nt1] [nt1]
globaltest4 = testtemplate [] [nt2]
globaltest5 = testtemplate [nt2] []
globaltest6 = testtemplate [nt2] [nt2]
globaltest7 = testtemplate [] [nt3]
globaltest8 = testtemplate [nt3] []
globaltest9 = testtemplate [nt3] [nt3]
globaltest10 = testtemplate [] [nt4]
globaltest11 = testtemplate [nt4] []
globaltest12 = testtemplate [nt4] [nt4]

globaltests = TestList [globaltest1, globaltest2, globaltest3,
                        globaltest4, globaltest5, globaltest6, 
                        globaltest7, globaltest8, globaltest9,
                        globaltest10, globaltest11, globaltest12]

testglobal  = runTestTT globaltests
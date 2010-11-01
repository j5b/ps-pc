{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: report.hs
   Description: provides a data type for checking which consist of a report and a result
-}

module Report where

type Success = Bool
type Log = String
type Report = (Log, Success)

buildReport :: String -> Bool -> Report
buildReport msg success = (msg, success)

printReport :: Report -> IO ()
printReport report = do putStrLn $ toOutput report

toOutput :: Report -> String
toOutput (txt, success) =
  (if success then "===== PASSED: =====\n" else "===== FAILED =====:\n") 
      ++ txt ++ "\n----- END REPORT -----"

standardReport :: Report
standardReport = ("No error found!", True)

combine :: Report -> Report -> Report
combine (txt1, success1) (txt2, success2) =
  (txt1++"\n----- END REPORT -----\n"++txt2, success1 && success2)

title :: String -> Report -> Report
title tt (txt, success) = (tt++"\n"++txt, success)

append :: String -> Report -> Report
append appMsg (txt, success) = (txt++"\n"++appMsg,success)
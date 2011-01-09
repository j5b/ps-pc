{- 
   Author: Michal Gerard Parusinski
   Maintainer: Michal Gerard Parusinski
   Email: <mparusinski@googlemail.com>
   License: GPL 3.0
   File: report.hs
   Description: provides a data type for checking which consist of a report and a result
-}

module Report (Report,
               buildReport,
               printReport,
               toOutput,
               standardReport,
               combine, 
               title,
               pushTitle) where

type Success = Bool
type Log = String
type Title = String
type Report = (Title, [Log], Success)

buildReport :: String -> Bool -> Report
buildReport msg success = ("", [msg], success)

printReport :: Report -> IO ()
printReport report = do putStrLn $ toOutput report

toOutput :: Report -> String
toOutput (title, txt, success) 
  | title == [] = mainMessage ++ newMsg
  | otherwise   = mainMessage ++ "=> "++title++"\n"++newMsg
  where mainMessage = if success then "===== PASSED: =====\n" else "===== FAILED =====:\n"
        newMsg      = (foldl (++) "" $ map (flip (++) "\n") txt)

standardReport :: Report
standardReport = ("Standard message", ["No error found!"], True)

combine :: Report -> Report -> Report
combine (title1, txt1, success1) (title2, txt2, success2) 
  | title1 == title2 = (title1, txt1++txt2, success1 && success2)
  | otherwise        = error "To combine reports we need the same title"

title :: String -> Report -> Report
title str (_, txt, success) = (str, txt, success)

pushTitle :: Report -> Report
pushTitle (title, msg, success) = ("", ["=> "++title++"\n"++newMsg], success)
  where newMsg = foldl (++) "" $ map (flip (++) "\n") $ (map ((++) "\t") msg)

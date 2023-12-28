module Lib
    ( runFile,
      runPrompt,
      trim
    ) where
-- import Control.Monad (forever)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, dropWhile)
import System.IO

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


runFile :: String -> IO ()
runFile s = do
    contents <- readFile s
    run contents

runPrompt :: IO ()
runPrompt = do
    putStr "> " 
    line <- getLine
    if trim line == "" then
        return ()
    else do
        run line
        runPrompt


run :: String -> IO ()
run contents = do
    let ls = lines contents in
        mapM print ls
    return ()
    
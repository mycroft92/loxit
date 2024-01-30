module Lib
    ( runFile,
      runPrompt,
      trim
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
-- import System.IO
import Expr
import qualified Scanner  as S
import qualified Parser as P
import Evaluator

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


runFile :: String -> IO ()
runFile s = do
    contents <- readFile s
    run contents s

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    line <- getLine
    if trim line == "" then
        return ()
    else do
        run line "interp"
        runPrompt


run :: String -> String -> IO ()
run contents fn = do
    let res = S.parse fn contents in
        case res of
            Left err -> print $ show err
            Right result -> -- mapM_ print result, tokenizer result
                do
                    let parseres = P.parse result in
                        case parseres of
                            Left err -> print err
                            Right expr ->
                                do
                                    x <- runInterpreter expr
                                    process x
    where 
        process (Left e) = print e
        process (Right x) = print x

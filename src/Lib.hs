module Lib
    ( runFile,
      runPrompt,
      trim
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
-- import System.IO

import qualified Scanner  as S
import qualified Parser as P
import Evaluator (runInterpreter, initState, InterpreterState)


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


runFile :: String -> IO Int
runFile s = do
    env <- initState
    contents <- readFile s
    run contents s env

runPrompt :: IO ()
runPrompt = do
    st <- initState
    handler st
    where
        handler ev = do
            putStr "> "
            line <- getLine
            if trim line == "" then
                return ()
            else do
                _ <- run line "interp" ev
                handler ev

run :: String -> String -> InterpreterState -> IO Int
run contents fn env = handler $ do --This is an either monad
    res <- S.parse fn contents
    P.parse res
    where --IO monad comes here
        handler (Left es)     = mapM_ print es >> return 1
        handler (Right expr) = do 
            x <- runInterpreter expr env
            print x
            case x of
                Left _ -> return 1
                _ -> return 0

    

-- run :: String -> String -> IO ()
-- run contents fn = do
--     let res = S.parse fn contents in
--         case res of
--             Left err -> print $ show err
--             Right result -> -- mapM_ print result, tokenizer result
--                 do
--                     let parseres = P.parse result in
--                         case parseres of
--                             Left err -> print err
--                             Right expr ->
--                                 do
--                                     x <- runInterpreter expr
--                                     process x
--     where 
--         process (Left e) = print e
--         process (Right x) = print x

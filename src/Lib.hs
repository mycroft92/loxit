module Lib
    ( runFile,
      runPrompt,
      trim
    ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
-- import System.IO
import Expr (Decl)
import qualified Scanner  as S
import qualified Parser as P
import Resolver
import Evaluator (runInterpreter, initState, interpret)
import Data.Map.Strict as Map (empty)


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


runFile :: String -> IO Int
runFile s = do
    
    contents <- readFile s
    decl <- runParser contents s
    case runResolver decl of
        Left err -> print err >> return 1
        Right m' -> do
            mapM_ print m'
            env <- initState m'
            res <- runInterpreter decl env
            case res of
                Left err -> print err >> return 1
                Right x  -> print x >> return 0

runPrompt :: IO ()
runPrompt = do
    st <- initState Map.empty
    handler st
    where
        handler ev = do
            putStr "> "
            line <- getLine
            if trim line == "" then
                return ()
            else do
                decl <- runParser line "interp"
                st' <- interpret decl ev
                handler st'

runParser :: String -> String -> IO [Decl]
runParser contents fn = handler $ do --This is an either monad
    res <- S.parse fn contents
    P.parse res
    where --IO monad comes here
        handler (Left es)     = mapM_ print es >> return []
        handler (Right expr) = return expr

    

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

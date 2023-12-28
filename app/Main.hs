module Main (main) where

import Lib (runFile, runPrompt)

import System.Environment
import System.IO
import qualified System.Exit as Exit

main :: IO ()
main = do
        args <- getArgs
        progName <- getProgName
        putStrLn  $ "Running: "++ progName

        if null args
            then do
                putStrLn "Interpreter mode:"
                hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering
                runPrompt
                Exit.exitSuccess
            else 
                if length args >= 1 then do
                    putStrLn $ "Running program "++ head args
                    runFile $ head args
                else do
                    putStrLn "Usage: loxit <file.lox>"
                    Exit.exitSuccess
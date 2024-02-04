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
                -- hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering
                runPrompt
                Exit.exitSuccess
            else
                if not (null args) then do
                    putStrLn $ "Running program "++ head args
                    x <- runFile $ head args
                    if x == 0 
                        then Exit.exitSuccess 
                        else Exit.exitFailure
                else do
                    putStrLn "Usage: loxit <file.lox>"
                    Exit.exitSuccess
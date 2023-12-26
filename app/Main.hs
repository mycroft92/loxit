module Main (main) where

import Lib

import System.Environment
import qualified System.Exit as Exit

main :: IO ()
main = do
        args <- getArgs
        progName <- getProgName
        putStrLn  $ "Running: "++ progName

        if length args < 2
            then do
                putStrLn "Usage: loxit <file.lox>"
                Exit.exitSuccess
            else
                putStrLn $ "Running program "++ (args !! 0)
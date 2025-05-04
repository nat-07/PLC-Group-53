module Main where

import Tokens
import Grammar
import Interpreter

import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, ErrorCall)

-- Main program
main :: IO ()
main = catch main' noParse

-- Main logic
main' :: IO ()
main' = do
    (fileName:_) <- getArgs
    sourceText <- readFile fileName
    putStrLn ("Parsing: " ++ sourceText)
    let parsedProg = parseCalc (alexScanTokens sourceText)
    putStrLn ("Parsed as: " ++ show parsedProg)
    interpret parsedProg
    return ()
-- Error handler for parsing issues
noParse :: ErrorCall -> IO ()
noParse e = hPutStrLn stderr $ "Error occurred: " ++ show e
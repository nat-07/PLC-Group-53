module Main where
    
import Tokens
import Grammar

import Interpreter (interpret)

import System.Environment
import Control.Exception
import System.IO


main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs 

           sourceText <- readFile fileName
           putStrLn ("Parsing : " ++ sourceText)
           let parsedProg = parseCalc (alexScanTokens sourceText)
           putStrLn ("Parsed as " ++ (show parsedProg))
           interpret parsedProg 

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()
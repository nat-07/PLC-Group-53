
    
import Tokens
import Grammar

import Interpreter (interpret)

import System.Environment
import Control.Exception
import System.IO

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import Control.Exception (catch, ErrorCall)

-- Main program
main :: IO String
main = catch main' noParse

-- Main logic
main' :: IO String
main' = do
    (fileName:_) <- getArgs
    sourceText <- readFile fileName
    putStrLn ("Parsing: " ++ sourceText)
    let parsedProg = parseCalc (alexScanTokens sourceText)
    putStrLn ("Parsed as: " ++ show parsedProg)
    result <- interpret parsedProg
    return $ show result

-- Error handler for parsing issues
noParse :: ErrorCall -> IO String
noParse e = do
    let err = "Error occurred: " ++ show e  -- Add context to the error
    hPutStr stderr err                    -- Print to stderr for error output
    return err
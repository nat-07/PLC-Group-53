module Interpreter where

import Grammar

import Data.List.Split (splitOn)
import Prelude 
import Data.List (intercalate)
import GHC.IO.Exception (IOException(ioe_filename))
import Prelude (putStrLn)
import Text.ParserCombinators.ReadP (string)


interpret :: Exp2 -> IO String

-- SELECT -- 
interpret (SELECT x) = do
    putStrLn "Running SELECT"
    column <- interpret x
    writeFile "output.csv" column
    putStrLn "Wrote result to output.csv"
    return column

-- DO -- 
interpret (DO task) = do
    putStrLn "Running DO"
    column <- interpret task
    writeFile "output.csv" column
    putStrLn "Wrote result to output.csv"
    return column


-- GET --
interpret (GET col csvExpr) = do
    putStrLn "Running GET"
    filename <- interpret csvExpr
    contents <- readFile filename
    let rows = map (splitOn ",") (lines contents)
    let colIndex = getColumnIndex col
    let colValues = [ row !! colIndex | row <- rows, length row > colIndex ]
    putStrLn ("Successfully got values: " ++ unlines colValues)
    return $ unlines colValues

interpret (FILENAME name) = return name

-- STR -- 
interpret (STR s) =
    return $ "String: " ++ s

-- ANDSELECT --
-- Left Most Inner Most Associativity 
interpret (ANDSELECT x y) = do
    putStrLn "Selecting the following items together:"
    columns <- interpret x
    nextColumn <- interpret y
    let merged = mergeColumns columns nextColumn
    putStrLn $ "New merged:\n" ++ merged
    return merged

-- THENTASK --
-- Left Most Inner Most Associativity 
interpret (THENTASK tasklist task) = do
    putStrLn "Doing tasks in order:"
    interpret tasklist
    interpret task
    return "Tasks done."

-- PRODUCT --
interpret (PRODUCT x y) = do
    putStrLn "Running PRODUCT Task:"
    table1 <- interpret x
    table2 <- interpret y
    let product = cartesianProduct table1 table2
    putStrLn $ "New Product:\n" ++ product
    return product

-- DROP --
interpret (DROP col file) = do 
    putStrLn $ "Running DROP Task: Dropping column " ++ show col ++ " in file " ++ show file
    filename <- interpret file
    contents <- Prelude.readFile filename
    let rows = lines contents
    let index = getColumnIndex col
    let droppedLines = map (dropColumn index) rows
    let result = unlines droppedLines
    putStrLn "Dropped column successfully."
    return result

-- PERMUTE -- 
interpret (PERMUTE x y) = do 
    return "Running PERMUTE Task:"
    interpret x
    interpret y
    return "Permutation finished"

-- COPY a string to a file --
interpret (COPY file string) = do 
    return "Running COPY Task:"
    filename <- interpret file 
    contents <- Prelude.readFile filename
    let rows = lines contents 
    copiedRows <- copyString rows string []
    putStrLn "Copy finished"
    return copied rows

-- COPYIN a string to a file of a speciifc column  --
interpret (COPYIN col file string) = do 
    return "Running COPY Task:"
    interpret col
    interpret file
    interpret string
    return "copy finished"

-- COPYIN a string to a file of a speciifc column  --
interpret (LEFTMERGEON file1 file2 col) = do 
    return "Running LEFT MERGE Task:"
    interpret file1
    interpret file2
    interpret col
    return "LEFT MERGE finished"

-- LEFT MERGE two files
interpret (LEFTMERGECSV file1 file2) = do
    return "Running LEFT MERGE Task:"
    interpret file1
    interpret file2
    return "LEFT MERGE finished"

-- LEFT MERGE two files of a specific column --
interpret (LEFTMERGE file1 file2 col) = do
    return "Running LEFT MERGE Task:"
    interpret file1
    interpret file2
    interpret col
    return "LEFT MERGE finished"

-- Conditions for tasks --
interpret (DOWHERE tasks condition) = do
    interpret tasks
    return "with Condition:"
    interpret condition
    return "Conditions applied"

-- Conditions for Select statements --
interpret (SELECTWHERE selects condition) = do
    interpret selects
    return "with Condition:"
    interpret condition
    return "Conditions applied"

-- Conditions when equalling two items together --
interpret (CONDITIONEQITEM item1 item2) = do 
    return "Condition that the following are equal to each other"
    interpret item1
    interpret item2

-- Conditions an item is equal to a particular string --
interpret (CONDITIONEQSTRING item1 item2) = do
    return "Condition that the following are equal to each other"
    interpret item1
    interpret item2

-- Conditions an item is NOT equal to a particular string  --
interpret (CONDITIONNEQSTRING item1 item2) = do
    return "Condition that the following are not equal to each other"
    interpret item1
    interpret item2

-- Conditions an item is empty  --
interpret (CONDITIONISEMPTY item) = do
    return "Condition that the following is empty"
    interpret item

-- Conditions an item is not empty  --
interpret (CONDITIONISNOTEMPTY item) = do
    return "Condition that the following is not empty"
    interpret item

-- AND  --
interpret (AND cond1 cond2) = do
    interpret cond1
    return "AND"
    interpret cond2
-- OR  --
interpret (OR cond1 cond2) = do
    interpret cond1
    return "OR"
    interpret cond2

interpret (RESULTOF task) = do
    interpret task




interpret expr = return $ "Unimplemented: " ++ show expr

getColumnIndex :: Exp2 -> Int
getColumnIndex (COLUMN i) = i-1
getColumnIndex _ = error "Expected a COLUMN expression with an index."

getFileName :: Exp2 -> String
getFileName (FILENAME name) = name
getFileName _ = error "Expected a FILE expression with a valid filename."

mergeColumns :: String -> String -> String
mergeColumns colA colB =
    let rowsA = lines colA
        rowsB = lines colB
        paired = zip rowsA rowsB
    in unlines [a ++ "," ++ b | (a, b) <- paired]

cartesianProduct :: String -> String -> String
cartesianProduct tableA tableB =
    let rowsA = lines tableA
        rowsB = lines tableB
        result = [a ++ "," ++ b | a <- rowsA, b <- rowsB]
    in unlines result

dropColumn :: Int -> String -> String 
dropColumn index line = 
    let columns = splitOn "," line
    in intercalate "," (take index columns ++ drop (index+1) columns)


copyString :: [String] -> String -> [String] -> [String]
copyString (r:rs) string copiedRows = 
    copyString rs string (copiedRows ++ [r ++ string ++ r])
copyString (r:[]) string copiedRows = copiedRows ++ [r ++ string ++ r]

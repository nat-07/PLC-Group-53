module Interpreter where
import System.Exit (exitFailure) 
import Grammar
import Data.List.Split (splitOn)
import Prelude 
import Debug.Trace (trace)
import Data.List (intercalate, dropWhileEnd)
import GHC.IO.Exception (IOException(ioe_filename))
import Text.ParserCombinators.ReadP (string)
import qualified Data.Map.Strict as Map
import Data.Bits (Bits(xor))
import Data.Char (isSpace)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

type Row = [String]

selectFromRow :: Exp2 -> Row -> String
selectFromRow (GET col _) row =
    let idx = getColumnIndex col
    in if idx < length row then row !! idx else ""

selectFromRow (ANDSELECT x y) row =
    let left = selectFromRow x row
        right = selectFromRow y row
    in left ++ "," ++ right

selectFromRow _ _ = ""


interpret :: Exp2 -> IO String


-- ANDQUERY --
-- Left Most Inner Most Associativity 
interpret (ANDQUERY x y) = do
    trace ("Querying " ++ show x) (return ())
    interpret x
    interpret y

-- SELECT -- 
interpret (SELECT x filename) = do
    column <- interpret x
    file <- interpret filename
    writeFile (show file) column
    return column

-- DO -- 
interpret (DO task filename) = do
    column <- interpret task
    file <- interpret filename
    let cleaned = intercalate "\n" (filter (not . null) (lines column))
    writeFile file cleaned
    return cleaned


-- GET --
interpret (GET col csvExpr) = do
    trace "Running GET" (return ())
    filename <- interpret csvExpr
    contents <- readFile filename
    validatedContents <- validateCsvFile contents
    let rows = map (splitOn ",") (lines validatedContents)
    let colIndex = getColumnIndex col
    let colValues = [ row !! colIndex | row <- rows, length row > colIndex ]
    trace ("Successfully got values: " ++ intercalate "\n" colValues) (return ())
    return $ intercalate "\n" colValues


interpret (FILENAME name) = return name

-- STR -- 
interpret (STR s) =
    return s

interpret (PRINT x) = do
    filename <- interpret x
    raw <- readFile filename
    let contents = intercalate "\n" . filter (not . null) . lines $ raw
    trace ("Printing contents of " ++ filename) (return ())
    putStrLn contents
    return contents
    
-- ANDSELECT --
-- Left Most Inner Most Associativity 
interpret (ANDSELECT x y) = do
    trace "Selecting the following items together:" (return ())
    columns <- interpret x
    nextColumn <- interpret y
    let merged = mergeColumns columns nextColumn
    trace ("New merged:\n" ++ merged) (return ())
    return merged

-- THENTASK --
-- Left Most Inner Most Associativity 
interpret (THENTASK tasklist task) = do
    trace "Doing tasks in order:" (return ())
    interpret tasklist
    interpret task
    return "Tasks done."

-- PRODUCT --
interpret (PRODUCT x y) = do
    trace "Running PRODUCT Task:" (return ())
    table1 <- interpret x
    table2 <- interpret y
    let product = cartesianProduct table1 table2
    trace ("New Product:\n" ++ product) (return ())
    return product

-- DROP --
interpret (DROP col file) = do 
    trace ("Running DROP Task: Dropping column " ++ show col ++ " in file " ++ show file) (return ())
    filename <- interpret file
    contents <- readFile filename
    validatedContents <- validateCsvFile contents
    let rows = lines validatedContents
    let index = getColumnIndex col
    let droppedLines = map (dropColumn index) rows
    let result = intercalate "\n" droppedLines
    trace "Dropped column successfully." (return ())
    return result

-- PERMUTE -- 
interpret (PERMUTE x) = do 
    trace "Running PERMUTE Task:" (return ())
    interpret x
    return "Permutation finished"

-- COPY a string to a file --
interpret (COPY file string) = do
    trace "Running COPY Task:" (return ())
    filename <- interpret file
    newString <- interpret string
    contents <- readFile filename
    validatedContents <- validateCsvFile contents
    let rows = lines validatedContents 
    let copiedRows = copyString rows newString
    trace "Copy finished" (return ())
    return (intercalate "\n" copiedRows)

-- COPYIN a string to a file of a speciifc column  --
interpret (COPYIN col file string) = do 
    trace "Running COPY Task:" (return ())
    interpret col
    interpret file
    interpret string
    return "copy finished"


-- COPYIN a string to a file of a speciifc column  --
interpret (LEFTMERGEON file1 file2 col) = do 
    let keyIndex = getColumnIndex col
    leftMerge (interpret file1) (interpret file2) keyIndex

-- LEFT MERGE two files
interpret (LEFTMERGECSV file1 file2) = do
    leftMerge (interpret file1) (interpret file2) 0

-- LEFT MERGE two files of a specific column --
interpret (LEFTMERGE file1 file2 col) = do
    trace "Running LEFT MERGE Task (LeftMerge):" (return ())

    leftContents <- interpret file1
    rightContents <- interpret file2

    let keyIndex = getColumnIndex col
        result = leftMergeFill keyIndex leftContents rightContents
    
    trace "Left merge finished" (return ())
    return result

-- Conditions for tasks --
interpret (DOWHERE task cond filename) = do
    trace "Running DOWHERE with condition" (return ())

    fname <- interpret filename
    contents <- readFile fname
    validatedContents <- validateCsvFile contents
    let rows = lines validatedContents
        rowData = map (splitOn ",") rows

    filtered <- filterRows cond rowData

    let result = intercalate "\n" (map (intercalate ",") filtered)
    return result

-- Conditions for Select statements --
interpret (SELECTWHERE selectExpr cond filename) = do
    trace "Running SELECTWHERE" (return ())
    file <- interpret filename
    contents <- interpret selectExpr
    let rows = lines contents
        rowData = map (splitOn ",") rows
    filtered <- filterRows cond rowData
    -- Evaluate the selection expression only on filtered rows
    let selected = map (selectFromRow selectExpr) filtered
    writeFile file (intercalate "\n" selected)
    trace ("Wrote result to " ++ file) (return ())
    return $ intercalate "\n" selected


interpret expr = return $ "Unimplemented: " ++ show expr
evaluateCondition :: Exp2 -> Row -> Bool
evaluateCondition (CONDITIONEQSTRING (COLUMN colIndexExpr) (STR value)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && row !! i == value
    in trace ("[EQSTRING] Checking if column " ++ show i ++ " == " ++ show value ++ ": " ++ show result) result

evaluateCondition (CONDITIONNEQSTRING (COLUMN colIndexExpr) (STR value)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && row !! i /= value
    in trace ("[NEQSTRING] Checking if column " ++ show i ++ " /= " ++ show value ++ ": " ++ show result) result

evaluateCondition (CONDITIONISEMPTY (COLUMN colIndexExpr)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && (null (trim (row !! i)))
    in trace ("[ISNOTEMPTY] Checking if Column " ++ show colIndexExpr ++ " is empty: " ++ show result) result

evaluateCondition (CONDITIONEQCOLUMN (COLUMN colIndexExpr1) (COLUMN colIndexExpr2)) row =
    let i1 = getColumnIndex (COLUMN colIndexExpr1)
        i2 = getColumnIndex (COLUMN colIndexExpr2)
        result = i1 < length row && i2 < length row && (row !! i1) == (row !! i2)
    in trace ("[EQCOLUMN] Comparing columns " ++ show i1 ++ " and " ++ show i2 ++ ": " ++ show result) result

evaluateCondition (CONDITIONNEQCOLUMN (COLUMN colIndexExpr1) (COLUMN colIndexExpr2)) row =
    let i1 = getColumnIndex (COLUMN colIndexExpr1)
        i2 = getColumnIndex (COLUMN colIndexExpr2)
        result = i1 < length row && i2 < length row && (row !! i1) /= (row !! i2)
    in trace ("[NEQCOLUMN] Comparing columns " ++ show i1 ++ " and " ++ show i2 ++ ": " ++ show result) result

evaluateCondition (CONDITIONISNOTEMPTY (COLUMN colIndexExpr)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && not (null (trim (row !! i)))
    in trace ("[ISNOTEMPTY] Checking if Column " ++ show colIndexExpr ++ " is not empty: " ++ show result) result

evaluateCondition (AND cond1 cond2) row =
    let r1 = evaluateCondition cond1 row
        r2 = evaluateCondition cond2 row
        result = r1 && r2
    in trace ("[AND] " ++ show r1 ++ " AND " ++ show r2 ++ ": " ++ show result) result

evaluateCondition (OR cond1 cond2) row =
    let r1 = evaluateCondition cond1 row
        r2 = evaluateCondition cond2 row
        result = r1 || r2
    in trace ("[OR] " ++ show r1 ++ " OR " ++ show r2 ++ ": " ++ show result) result

evaluateCondition _ _ = trace "[DEFAULT] Condition did not match any known pattern. Returning False." False


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
    in (intercalate "\n" [a ++ "," ++ b | (a, b) <- paired])

cartesianProduct :: String -> String -> String
cartesianProduct tableA tableB =
    let rowsA = lines tableA
        rowsB = lines tableB
        result = [a ++ "," ++ b | a <- rowsA, b <- rowsB]
    in (intercalate "\n" result)

dropColumn :: Int -> String -> String 
dropColumn index line = 
    let columns = splitOn "," line
    in (intercalate "," (take index columns ++ drop (index+1) columns))
    
leftMerge :: IO String -> IO String -> Int -> IO String
leftMerge file1 file2 keyIndex = do
    putStrLn $ "Running LEFTMERGE on column " ++ show (keyIndex+1)

    file1Name <- file1
    file2Name <- file2

    leftContents <- readFile file1Name
    validatedLeftContents <- validateCsvFile leftContents
    rightContents <- readFile file2Name
    validatedRightContents <- validateCsvFile rightContents


    putStrLn ""
    putStrLn "left contents: "
    putStrLn validatedLeftContents

    putStrLn ""
    putStrLn "right contents: "
    putStrLn validatedRightContents
    putStrLn ""

    let result = leftMergeFill keyIndex validatedLeftContents validatedRightContents
    putStrLn "Left merge finished"
    return result


leftMergeFill :: Int -> String -> String -> String
leftMergeFill keyIndex leftStr rightStr = 
    let leftRows = lines leftStr
        rightRows = lines rightStr
        leftColumns = map (splitOn ",") leftRows
        rightColumns = map (splitOn ",") rightRows

        mapRight = Map.fromListWith (++) [(getKey row keyIndex, [row]) | row <- rightColumns, length row > keyIndex]
        results = concatMap (\row -> mergeWithRight mapRight row keyIndex) leftColumns

    in intercalate "\n" $ map (intercalate ",") results

mergeWithRight :: Map.Map String [[String]] -> [String] -> Int -> [[String]]
mergeWithRight rightMap leftRow keyIndex = 
    let key = getKey leftRow keyIndex
    in case Map.lookup key rightMap of
        Nothing -> []
        Just rightRows -> [zipWith fillEmpty leftRow rightRow | rightRow <- rightRows]

getKey :: [String] -> Int -> String
getKey row i = if i < length row then row !! i else ""

fillEmpty :: String -> String -> String
fillEmpty p q = if null p then q else p

copyString :: [String] -> String -> [String]
copyString [] _ = []
copyString (r:rs) string = (r ++ "," ++ string ++ "," ++ r) : copyString rs string

filterRows :: Exp2 -> [Row] -> IO [Row]
filterRows cond rows = do
    return [row | row <- rows, evaluateCondition cond row]

validateCsvFile :: String -> IO String
validateCsvFile contents
    | null contents = do
        trace "File is empty. Stopping execution." (return ())
        exitFailure
    | otherwise = do
        let rows = lines contents
        let parsedRows = map (splitOn ",") rows
        let arities = map length parsedRows
        if not (all (== head arities) (tail arities))
            then do
                trace "Invalid CSV format: inconsistent number of fields." (return ())
                exitFailure
            else return contents  -- Return the raw string after validation

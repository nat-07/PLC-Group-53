module Interpreter where
import System.Exit (exitFailure) 
import Grammar
import Data.List (sort)
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
    interpret x
    interpret y


-- SELECT--
interpret (SELECT task filename) = do
    column <- interpret task
    file <- interpret filename

    let cleanedLines = filter (not . null) (lines column)
        cleaned = intercalate "\n" cleanedLines
        finalOutput = sortCsv cleaned
        finalOutput' = if not (null finalOutput) && last finalOutput == '\n'
                       then init finalOutput
                       else finalOutput

    writeFile file finalOutput'
    return finalOutput'

-- DO -- 
interpret (DO task filename) = do
    column <- interpret task
    file <- interpret filename

    let cleanedLines = filter (not . null) (lines column)
        cleaned = intercalate "\n" cleanedLines
        finalOutput = sortCsv cleaned
        finalOutput' = if not (null finalOutput) && last finalOutput == '\n'
                       then init finalOutput
                       else finalOutput

    writeFile file finalOutput'
    return finalOutput'

-- Interpret GET operation
interpret (GET col csvExpr) = do
    filename <- interpret csvExpr
    contents <- readFile filename
    validatedContents <- validateCsvFile contents
    let rows = map (splitOn ",") (lines validatedContents)
    let colIndex = getColumnIndex col    
    let colValues = [ if length row > colIndex then row !! colIndex else "" | row <- rows ]
    let newColValues = map (\x -> if x == "" then " " else x) colValues
    -- Check if colValues is empty and return " " if it is
    if null newColValues
        then do
            return " "
        else do
            return $ intercalate "\n" newColValues

interpret (FILENAME name) = return name

-- STR -- 
interpret (STR s) =
    return s

interpret (PRINT x) = do
    filename <- interpret x
    raw <- readFile filename
    let contents = intercalate "\n" . filter (not . null) . lines $ raw
    if null contents
        then 
        return contents
        else do
            putStrLn contents
            return contents
    
-- ANDSELECT --
-- Left Most Inner Most Associativity 
interpret (ANDSELECT x y) = do
    columns <- interpret x
    nextColumn <- interpret y
    let merged = mergeColumns columns nextColumn
    return merged

-- READ --
interpret (GETCSV filename) = do
    file <- interpret filename
    contents <- readFile file
    validatedContents <- validateCsvFile contents
    let rows = map (splitOn ",") (lines validatedContents)    
    let cleanedRows = map (map (\x -> if null x then " " else x)) rows
    let reconstructedLines = map (intercalate ",") cleanedRows
    return (intercalate "\n" reconstructedLines)


-- PRODUCT --
interpret (PRODUCT x y) = do
    table1 <- interpret x
    table2 <- interpret y
    let product = cartesianProduct table1 table2
    return product

-- DROP --
interpret (DROP col file) = do 
    contents <- interpret file
    let rows = lines contents
    let index = getColumnIndex col
    let droppedLines = map (dropColumn index) rows
    let result = intercalate "\n" droppedLines
    return result

-- COPY a string to a file --
interpret (COPY file string) = do
    contents <- interpret file
    newString <- interpret string
    let rows = lines contents 
    let copiedRows = copyString rows newString
    return (intercalate "\n" copiedRows)

interpret (COPYEMPTYCOL file) = do
    contents <- interpret file
    let rows = lines contents 
    let copiedRows = copyString rows " "
    return (intercalate "\n" copiedRows)

-- LEFT MERGE two files of a specific column --
interpret (LEFTMERGE file1 file2 col) = do
    leftContents <- interpret file1
    rightContents <- interpret file2
    let keyIndex = getColumnIndex col
        result = leftMergeFill keyIndex leftContents rightContents
    return result

-- Conditions for tasks --
interpret (DOWHERE task cond filename) = do
    file <- interpret filename
    contents <- interpret task
    let rows = lines contents
        rowData = map (splitOn ",") rows
    filtered <- filterRows cond rowData
    writeFile file (sortCsv (intercalate "\n" filtered))
    return $ intercalate "\n" filtered

-- Conditions for Select statements --
interpret (SELECTWHERE selectExpr cond filename) = do
    file <- interpret filename
    contents <- interpret selectExpr
    let rows = lines contents
        rowData = map (splitOn ",") rows
    filtered <- filterRows cond rowData
    writeFile file (sortCsv (intercalate "\n" filtered))
    return $ intercalate "\n" filtered

interpret expr = return $ "Unimplemented: " ++ show expr
evaluateCondition :: Exp2 -> Row -> Bool
evaluateCondition (CONDITIONEQSTRING (COLUMN colIndexExpr) (STR value)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && row !! i == value
    in result

evaluateCondition (CONDITIONNEQSTRING (COLUMN colIndexExpr) (STR value)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && row !! i /= value
    in result

evaluateCondition (CONDITIONISEMPTY (COLUMN colIndexExpr)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && (null (trim (row !! i)))
    in result

evaluateCondition (CONDITIONEQCOLUMN (COLUMN colIndexExpr1) (COLUMN colIndexExpr2)) row =
    let i1 = getColumnIndex (COLUMN colIndexExpr1)
        i2 = getColumnIndex (COLUMN colIndexExpr2)
        result = i1 < length row && i2 < length row && (row !! i1) == (row !! i2)
    in result

evaluateCondition (CONDITIONNEQCOLUMN (COLUMN colIndexExpr1) (COLUMN colIndexExpr2)) row =
    let i1 = getColumnIndex (COLUMN colIndexExpr1)
        i2 = getColumnIndex (COLUMN colIndexExpr2)
        result = i1 < length row && i2 < length row && (row !! i1) /= (row !! i2)
    in result

evaluateCondition (CONDITIONISNOTEMPTY (COLUMN colIndexExpr)) row =
    let i = getColumnIndex (COLUMN colIndexExpr)
        result = i < length row && not (null (trim (row !! i)))
    in result

evaluateCondition (AND cond1 cond2) row =
    let r1 = evaluateCondition cond1 row
        r2 = evaluateCondition cond2 row
        result = r1 && r2
    in result

evaluateCondition (OR cond1 cond2) row =
    let r1 = evaluateCondition cond1 row
        r2 = evaluateCondition cond2 row
        result = r1 || r2
    in result

evaluateCondition _ _ = False


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
    file1Name <- file1
    file2Name <- file2

    leftContents <- readFile file1Name
    validatedLeftContents <- validateCsvFile leftContents
    rightContents <- readFile file2Name
    validatedRightContents <- validateCsvFile rightContents
    let result = leftMergeFill keyIndex validatedLeftContents validatedRightContents
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
copyString (r:rs) string = (r ++ "," ++ string) : copyString rs string

filterRows :: Exp2 -> [Row] -> IO [[Char]]
filterRows cond rows = do
    let filtered = [row | row <- rows, evaluateCondition cond row]
    let csvLines = map (intercalate ",") filtered
    return csvLines

-- Apply trim to every field in a row
cleanRow :: String -> String
cleanRow row = intercalate "," $ map trim $ splitOn "," row

validateCsvFile :: String -> IO String
validateCsvFile contents
    | null contents = do
        error "File is empty. Stopping execution."
        exitFailure
    | otherwise = do
        -- Check if one-column CSV
        let rawRows = lines contents
        let cleanedRows = map (map trim . splitOn ",") rawRows

        -- Special case: one-column + ends with newline (missing row after final \n)
        let isOneColumn = all ((== 1) . length) cleanedRows
        let endsWithNewline = not (null contents) && last contents == '\n'

        let finalRows =
              if isOneColumn && endsWithNewline
                 then cleanedRows ++ [[""]]  -- Add an empty row for the trailing newline
                 else cleanedRows


        -- Check if CSV with multiple columns ends with an empty line
        let hasMultipleColumns = any ((> 1) . length) finalRows
        let endsWithNewline = not (null contents) && last contents == '\n'

        if hasMultipleColumns && endsWithNewline
            then do
                error "Invalid CSV format: multiple columns with an empty line at the end."
                exitFailure
            else do
                let arities = map length finalRows
                if not (all (== head arities) (tail arities))
                    then do
                        error "Invalid CSV format: inconsistent number of fields."
                        exitFailure
                    else return $ unlines (map (intercalate ",") finalRows)
-- Split on comma *manually* to allow whitespace trimming
splitCsvLine :: String -> [String]
splitCsvLine [] = [""]
splitCsvLine (c:cs)
    | c == ','  = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = splitCsvLine cs

-- Trim all fields in a row
trimRowFields :: String -> String
trimRowFields row =
    let fields = splitCsvLine row
    in intercalate "," (map trim fields)

-- Sort all rows after trimming fields
sortCsv :: String -> String
sortCsv contents =
    let rows = lines contents
        cleaned = map trimRowFields rows
        sorted = sort cleaned
    in unlines sorted
module Interpreter where

import Grammar

interpret :: Exp2 -> IO ()
interpret expr = case expr of
    SELECT x         -> putStrLn "Running SELECT" >> interpret x
    DO x             -> putStrLn "Running DO" >> interpret x
    GET col csv      -> putStrLn $ "Getting: " ++ show col ++ " from " ++ show csv
    FILENAME name    -> putStrLn $ "Filename: " ++ name
    COLUMN n         -> putStrLn $ "Column Index: " ++ show n
    STR s            -> putStrLn $ "String: " ++ s
    ANDSELECT x y    -> do
        putStrLn "Selecting the following items together:"
        interpret x  -- Interpret the first select item
        interpret y  -- Interpret the second select item
        putStrLn "Done selecting."
    THENTASK x y    -> do
        putStrLn "Doing tasks in order:"
        interpret x  -- Interpret the first select item
        interpret y  -- Interpret the second select item
        putStrLn "Tasks done."
    PRODUCT x y    -> do
        putStrLn "Running PRODUCT Task:"
        interpret x  -- Interpret the first select item
        interpret y  -- Interpret the second select item
        putStrLn "Combing the two tables together"
    DROP col file    -> putStrLn $ "Running DO Task: Dropping column" ++ show col ++ " in " ++ show file
    PERMUTE x y    -> do 
        putStrLn "Running PERMUTE Task:"
        interpret x  -- Interpret the first select item
        interpret y  -- Interpret the second select item
        putStrLn "Permutation finished"
    COPY file string       -> do 
        putStrLn "Running COPY Task:"
        interpret file  -- Interpret the first select item
        interpret string  -- Interpret the second select item
        putStrLn "Copy finished"
    COPYIN col file string       -> do 
        putStrLn "Running COPY Task:"
        interpret col  -- Interpret the first select item
        interpret file  -- Interpret the second select item
        interpret string
        putStrLn "copy finished"
    LEFTMERGEON file1 file2 col -> do 
        putStrLn "Running LEFT MERGE Task:"
        interpret file1  -- Interpret the first select item
        interpret file2  -- Interpret the second select item
        interpret col
        putStrLn "LEFT MERGE finished"
    LEFTMERGECSV file1 file2 -> do
        putStrLn "Running LEFT MERGE Task:"
        interpret file1  -- Interpret the first select item
        interpret file2  -- Interpret the second select item
        putStrLn "LEFT MERGE finished"
    LEFTMERGE file1 file2 col -> do
        putStrLn "Running LEFT MERGE Task:"
        interpret file1  -- Interpret the first select item
        interpret file2  -- Interpret the second select item
        interpret col
        putStrLn "LEFT MERGE finished"
    DOWHERE tasks condition -> do
        interpret tasks  -- Interpret the first select item
        putStrLn "with Condition:"
        interpret condition
        putStrLn "Tasks done"
    SELECTWHERE selects condition -> do
        interpret selects  -- Interpret the first select item
        putStrLn "with Condition:"
        interpret condition
        putStrLn "Conditions applied"
    CONDITIONEQITEM x y -> do 
        putStrLn "Condition that the following are equal to each other"
        interpret x  -- Interpret the first select item
        interpret y  -- Interpret the second select item
    _                -> putStrLn $ "Unimplemented: " ++ show expr

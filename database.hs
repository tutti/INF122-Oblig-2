module Database where

import System.IO
import System.Directory
import Data.List

filename :: String -> String
filename s = "dbs/"++s++".txt"

type DB = (String, [String])

saveDB :: DB -> [[String]] -> IO ()
saveDB (name, colLabels) rows = do
    {-let tmpname = ((filename name)++".tmp")
    exists <- doesFileExist tmpname
    if exists then removeFile tmpname else return ()
    (realtmpname, file) <- openTempFile "." tmpname
    hPutStrLn file $ intercalate "\0" colLabels
    let m = map (\row -> hPutStrLn file $ intercalate "\0" row) rows
    hClose file
    exists <- doesFileExist $ filename name
    if exists then removeFile $ filename name else return ()
    renameFile realtmpname $ filename name-}
    let colstr = intercalate "\0" colLabels
        rowstrs = map (intercalate "\0") rows
        rowstr = intercalate "\n" rowstrs
        fullstr = colstr++"\n"++rowstr
    writeFile (filename name) fullstr

readRow :: String -> [String]
readRow "" = []
readRow ('\0':xs) = readRow xs
readRow row = ((takeWhile (/= '\0') row):(readRow $ dropWhile (/= '\0') row))

readDB :: String -> IO (DB, [[String]])
readDB dbname = do
    _contents <- readFile $ filename dbname
    let contents = (length _contents) `seq` (lines _contents)
    return ((dbname, readRow $ head contents), map readRow $ tail contents)
    {-file <- openFile (filename dbname) ReadMode
    _contents <- hGetContents file
    let contents = lines _contents
    hClose file
    return ((dbname, readRow $ head contents), map readRow $ tail contents)-}

{-    withFile (filename dbname) ReadMode $ \file -> do
        _contents <- hGetContents file
        let contents = seq _contents $ lines _contents
        return ((dbname, readRow $ head contents), map readRow $ tail contents)-}

readCols :: String -> IO [String]
readCols dbname = do
{-    exists <- doesFileExist $ filename dbname
    if not exists then do putStrLn "[ERROR] Database doesn't exist."; return [] else do
    file <- openFile (filename dbname) ReadMode
    header <- hGetLine file
    hClose file
    return (readRow header)-}
    
    contents <- readFile $ filename dbname
    let header = (length contents) `seq` (head $ lines contents)
    return (readRow header)

    {-withFile (filename dbname) ReadMode $ \file -> do
        contents <- hGetContents file
        let header = head $ lines contents
        return (readRow header)-}

_askForColumns :: [String] -> IO [String]
_askForColumns cols = do
    col <- getLine
    if col == "" then return cols else _askForColumns (cols++[col])

askForColumns :: IO [String]
askForColumns = _askForColumns []

_askForValues :: [String] -> [String] -> IO [String]
_askForValues [] temp = return temp
_askForValues (name:names) temp = do
    putStrLn ("Enter a value for column "++name++":")
    value <- getLine
    _askForValues names (temp++[value])

askForValues :: [String] -> IO [String]
askForValues names = _askForValues names []

cmd_NYI :: IO ()
cmd_NYI = putStrLn "Not yet implemented."

cmd_createDB :: String -> IO ()
cmd_createDB dbname = do
    exists <- doesFileExist $ filename dbname
    if exists
    then putStrLn "[ERROR] A database by that name already exists."
    else do
        putStrLn "Enter your column names, one per line; end with empty line."
        cols <- askForColumns
        if length cols == (length $ nub cols)
        then do
            let db = (dbname, cols)
            saveDB db []
        else putStrLn "[ERROR] Duplicate column names are not allowed."

cmd_deleteDB :: String -> IO ()
cmd_deleteDB dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists
    then putStrLn "[ERROR] Database doesn't exist."
    else removeFile $ filename dbname

cmd_insert :: String -> IO ()
cmd_insert dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else do
    ((_, colnames), rows) <- readDB dbname
    newrow <- askForValues colnames
    saveDB (dbname, colnames) (rows++[newrow])

cmd_print :: String -> IO ()
cmd_print dbname = do
    (dbhead, rows) <- readDB dbname
    putStrLn $ show rows

main :: IO ()
main = do
    putStrLn "Please select an operation:"
    putStrLn "a: Create a database"
    putStrLn "b: Delete a database"
    putStrLn "c: Insert an entry into a database"
    putStrLn "d: Print a database"
    putStrLn "e: Select entries from a database"
    putStrLn "f: Delete entries from a database"
    putStrLn "g: Update entries in a database"
    putStrLn "q: Quit"
    choice <- getChar
    getLine
    if choice /= 'q' then do
        putStrLn "Please enter a database name:"
        dbname <- getLine
        case choice of
            'a' -> cmd_createDB dbname
            'b' -> cmd_deleteDB dbname
            'c' -> cmd_insert dbname
            'd' -> cmd_print dbname
            'e' -> cmd_NYI
            'f' -> cmd_NYI
            'g' -> cmd_NYI
            _ -> putStrLn "Unknown command."
        main
    else return ()
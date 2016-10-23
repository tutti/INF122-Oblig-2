module Database where

import System.IO
import System.Directory
import Data.List

filename :: String -> String
filename s = "dbs/"++s++".txt"

type DB = (String, [String])

saveDB :: DB -> [String] -> IO ()
saveDB (name, colLabels) rows = do
    let tmpname = ((filename name)++".tmp")
    exists <- doesFileExist tmpname
    if exists then removeFile tmpname else return ()
    file <- openFile tmpname WriteMode
    hPutStrLn file $ intercalate "\0" colLabels
    hClose file
    exists <- doesFileExist $ filename name
    if exists then removeFile $ filename name else return ()
    renameFile tmpname $ filename name

readRow :: String -> [String]
readRow "" = []
readRow row = ((takeWhile (/= '\0') row):(readRow $ dropWhile (/= '\0') row))

readDB :: String -> IO (DB, [[String]])
readDB dbname = do
    _contents <- readFile $ filename dbname
    let contents = lines _contents
    return ((dbname, readRow $ head contents), map readRow $ tail contents)

_askForColumns :: [String] -> IO [String]
_askForColumns cols = do
    col <- getLine
    if col == "" then return cols else _askForColumns (cols++[col])

askForColumns :: IO [String]
askForColumns = _askForColumns []

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

{-cmd_insert :: String -> IO ()
cmd_insert dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else
    -}

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
            'c' -> cmd_NYI
            'd' -> cmd_print dbname
            'e' -> cmd_NYI
            'f' -> cmd_NYI
            'g' -> cmd_NYI
            _ -> putStrLn "Unknown command."
        main
    else return ()
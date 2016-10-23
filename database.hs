module Database where

import System.IO
import System.Directory
import Data.List
import Data.Char

{-
    Navn: Pål Vårdal Gjerde

    Tilleggsopplysninger: Lag en mappe som heter "dbs" på samme sted som denne filen ligger.
    Det er inne i denne mappen databasefiler vil lagres.
    Alternativt kan funksjonen filename rett under her endres til å peke på en annen sti.
-}

filename :: String -> String
filename s = "dbs/"++s++".txt"

maxArrays :: [Int] -> [Int] -> [Int]
maxArrays first second = [max a b | (a, b) <- zip first second]

maxLengths :: [[String]] -> [Int]
maxLengths [] = []
maxLengths [single] = map length single
maxLengths (first:second:rest) = maxArrays (map length first) (maxLengths (second:rest))

type DB = (String, [String])

saveDB :: DB -> [[String]] -> IO ()
saveDB (name, colLabels) rows = do
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

getValue :: DB -> [String] -> String -> String
getValue (_, []) _ colname = error ("Column "++colname++" not found.")
getValue (dbname, (fcol:colnames)) (fval:row) colname = if fcol == colname then fval else getValue (dbname, colnames) row colname

setValue :: DB -> [String] -> String -> String -> [String]
setValue (_, []) _ colname _ = error ("Column "++colname++" not found.")
setValue (dbname, (fcol:colnames)) (fval:row) colname val = if fcol == colname then (val:row) else (fval:setValue (dbname, colnames) row colname val)

char = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

isChar :: Char -> Bool
isChar c = c `elem` char

isAllChar :: String -> Bool
isAllChar s = all isChar s

isInt :: String -> Bool
isInt s = all isDigit s

data Predicate =
      Colname String
    | Number Int
    | Literal String
    | Op Predicate String Predicate -- "+", "-", etc
    | Bool Predicate String Predicate -- "=", ">", "<", etc
    | Combine Predicate String Predicate -- "&", "|"
    deriving (Show, Eq, Ord)

tokenize :: String -> [String]
tokenize [] = []
tokenize (' ':rest) = tokenize rest
tokenize ('!':'=':rest) = "!=":tokenize rest
tokenize ('>':'=':rest) = ">=":tokenize rest
tokenize ('<':'=':rest) = "<=":tokenize rest
tokenize ('&':'&':rest) = "&":tokenize rest
tokenize ('|':'|':rest) = "|":tokenize rest
tokenize ('=':rest) = "=":tokenize rest
tokenize ('>':rest) = ">":tokenize rest
tokenize ('<':rest) = "<":tokenize rest
tokenize ('+':rest) = "+":tokenize rest
tokenize ('-':rest) = "-":tokenize rest
tokenize ('*':rest) = "*":tokenize rest
tokenize ('/':rest) = "/":tokenize rest
tokenize ('&':rest) = "&":tokenize rest
tokenize ('|':rest) = "|":tokenize rest
tokenize ('(':rest) = "(":tokenize rest
tokenize (')':rest) = ")":tokenize rest
tokenize ('"':rest) = "\"":(takeWhile (/= '"') rest):"\"":tokenize ( tail (dropWhile (/= '"') rest) )
tokenize (s:xs)
    | isDigit s = (takeWhile isDigit (s:xs)):tokenize (dropWhile isDigit xs)
    | isChar s = (takeWhile isChar (s:xs)):tokenize (dropWhile isChar xs)
    | otherwise = error ("Syntax error: Unexpected '"++[s]++"'.")

order4 = ["*", "/"]
order3 = ["+", "-"]
order2 = ["=", "!=", "<", "<=", ">", ">="]
order1 = ["&", "|"]

orderOf :: String -> Int
orderOf ")"  = 5
orderOf "*"  = 4
orderOf "/"  = 4
orderOf "+"  = 3
orderOf "-"  = 3
orderOf "="  = 2
orderOf "<"  = 2
orderOf ">"  = 2
orderOf "!=" = 2
orderOf "<=" = 2
orderOf ">=" = 2
orderOf "&"  = 1
orderOf "|"  = 1
orderOf "("  = -1
orderOf s = error ("Unknown token "++s++".")

combine :: Predicate -> String -> Predicate -> Predicate
combine p1 op p2
    | op `elem` order4 || op `elem` order3 = Op p1 op p2
    | op `elem` order2 = Bool p1 op p2
    | op `elem` order1 = Combine p1 op p2
    | otherwise = error "Unknown operator."

parse :: String -> Predicate
parse "" = Bool (Number 0) "=" (Number 0)
parse s =
    let
        {-
            NON-OBVIOUS TRICK:
            The parse recursion reads from left to right, in the manner of "create an operation out of this and the rest".
            This causes all the operations to fold right. However, this causes the calculations to be done right to left.
            To solve this, the token list is simply reversed before it's handed to the parser. The parser, in turn,
            is aware of this, and stores its operations in re-reversed order, and reads "(" and ")" as the other of the
            pair. The end result is a left folded list of operations done.

            Do note that the rules of precedence are also observed, not only the left to right order (i.e. multiplication
            happens before addition, and so on). This is in fact what made it difficult to use less trick-y methods of
            achieving left to right ordering.
        -}
        tokens = reverse $ tokenize s
        (predicate, _) = parsePredicate tokens 0
    in predicate

parsePredicate :: [String] -> Int -> (Predicate, [String])
parsePredicate (token:tokens) order =
    let
        (firstPredicate, rest) = if token == ")" then parseSub (token:tokens)
            else if token == "\"" then parseLiteral (token:tokens)
            else if isInt token then parseNumber (token:tokens)
            else parseColname (token:tokens)
    in if rest == [] then (firstPredicate, rest)
    else let
        next = head rest
        nextorder = orderOf next
    in if nextorder < order then (firstPredicate, rest)
    else let
        (secondPredicate, rest') = parsePredicate (tail rest) nextorder
    in if rest' == [] then (combine secondPredicate next firstPredicate, rest')
    else let
        next' = head rest'
    in if next' == "(" then (combine secondPredicate next firstPredicate, rest')
    else let
        nextorder' = orderOf next'
        (thirdPredicate, rest'') = parsePredicate (tail rest') nextorder'
    in (combine thirdPredicate next' (combine secondPredicate next firstPredicate), rest'')
parsePredicate s n = error $ show s

parseColname :: [String] -> (Predicate, [String])
parseColname (name:tokens) = (Colname name, tokens)
parseColname _ = error "Expected column name."

parseNumber :: [String] -> (Predicate, [String])
parseNumber (num:tokens)
    | isInt num = (Number (read num :: Int), tokens)
    | otherwise = error "Expected number."
parseNumber _ = error "Expected number."

parseLiteral :: [String] -> (Predicate, [String])
parseLiteral ("\"":name:"\"":tokens) = (Literal name, tokens)
parseLiteral _ = error "Expected a literal value."

parseSub :: [String] -> (Predicate, [String])
parseSub (")":sub) =
    let (predicate, rest) = parsePredicate sub 1
    in
        if rest /= [] && head rest == "(" then
            (predicate, tail rest)
        else error "Syntax error."

eval :: Predicate -> DB -> [String] -> Predicate
eval (Number n) _ _ = Number n
eval (Literal l) _ _ = Literal l
eval (Colname c) db row =
    let val = getValue db row c
    in if isInt val then Number (read val) else Literal val
eval (Op p1 op p2) db row = 
    let (Number val1) = eval p1 db row
        (Number val2) = eval p2 db row
    in case op of
        "+" -> Number (val1 + val2)
        "-" -> Number (val1 - val2)
        "*" -> Number (val1 * val2)
        "/" -> Number (val1 `div` val2)
        otherwise -> error "Illegal operator."
eval _ _ _ = error "Can't evaluate expression."

testPredicate :: Predicate -> DB -> [String] -> Bool
testPredicate (Bool p1 op p2) db row =
    let val1 = eval p1 db row
        val2 = eval p2 db row
    in case op of
        "=" -> val1 == val2
        ">" -> val1 > val2
        "<" -> val1 < val2
        "!=" -> val1 /= val2
        ">=" -> val1 >= val2
        "<=" -> val1 <= val2
        otherwise -> error "Can't evaluate boolean expression."
testPredicate (Combine p1 op p2) db row =
    let val1 = testPredicate p1 db row
        val2 = testPredicate p2 db row
    in case op of
        "&" -> val1 && val2
        "|" -> val1 || val2
        otherwise -> error "Can't evaluate combined expression."

readCols :: String -> IO [String]
readCols dbname = do
    contents <- readFile $ filename dbname
    let header = (length contents) `seq` (head $ lines contents)
    return (readRow header)

_askForColumns :: [String] -> IO [String]
_askForColumns cols = do
    col <- getLine
    if not $ isAllChar col then return ["::BAD_COLUMN", col]
    else if col == "" then return cols else _askForColumns (cols++[col])

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

pad :: String -> Int -> String
pad original maxlength = original++(replicate (maxlength - length original) ' ')

putRow :: [Int] -> [String] -> IO ()
putRow [] [] = error "Table has no columns."
putRow [] _ = error "Mismatch between number of lengths and rows"
putRow _ [] = error "Mismatch between number of lengths and rows"
putRow [lastLength] [lastValue] = do
    putStr $ pad lastValue lastLength
    putStrLn ""
putRow (firstLength:lengths) (firstValue:rest) = do
    putStr $ pad firstValue firstLength
    putStr " "
    putRow lengths rest

putRows :: [Int] -> [[String]] -> IO ()
putRows lengths [] = return ()
putRows lengths (first:rest) = do
    putRow lengths first
    putRows lengths rest

putRowSeparator :: [Int] -> IO ()
putRowSeparator lengths = putStrLn $ replicate (length lengths + sum lengths - 1) '-'

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
        if length cols == 0
        then putStrLn "[ERROR] Can't create table without columns."
        else if head cols == "::BAD_COLUMN"
        then putStrLn "[ERROR] Invalid column name."
        else if length cols == (length $ nub cols)
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
    let filteredrow = [field | field <- newrow, field /= "" ]
    if length filteredrow /= length colnames
    then putStrLn "[ERROR] You can't leave columns empty."
    else saveDB (dbname, colnames) (rows++[filteredrow])

cmd_print :: String -> IO ()
cmd_print dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else do
    ((_, colnames), rows) <- readDB dbname
    let lengths = maxLengths (colnames:rows)
    putRowSeparator lengths
    putRow lengths colnames
    putRowSeparator lengths
    putRows lengths rows
    putRowSeparator lengths

cmd_select :: String -> IO ()
cmd_select dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else do
    (db, rows) <- readDB dbname
    putStrLn "Enter a predicate:"
    predicateStr <- getLine
    let predicate = parse predicateStr
        selected = [row | row <- rows, testPredicate predicate db row]
        (_, colnames) = db
        lengths = maxLengths (colnames:selected)
    putRowSeparator lengths
    putRow lengths colnames
    putRowSeparator lengths
    putRows lengths selected
    putRowSeparator lengths

cmd_delete :: String -> IO ()
cmd_delete dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else do
    (db, rows) <- readDB dbname
    putStrLn "Enter a predicate:"
    predicateStr <- getLine
    let predicate = parse predicateStr
        remaining = [row | row <- rows, not $ testPredicate predicate db row]
    putStrLn ("Warning! This will delete "++(show $ length rows - length remaining)++" rows. Are you sure? [yes/no]")
    sure <- getLine
    if sure == "yes" then
        saveDB db remaining
    else return ()

cmd_update :: String -> IO ()
cmd_update dbname = do
    exists <- doesFileExist $ filename dbname
    if not exists then putStrLn "[ERROR] Database doesn't exist." else do
    (db, rows) <- readDB dbname
    putStrLn "Enter a predicate for which rows to update:"
    predicateStr <- getLine
    let predicate = parse predicateStr
        (_, colnames) = db
        changing = [row | row <- rows, testPredicate predicate db row]
    putStrLn ("Warning! This will update "++(show $ length changing)++" rows. Are you sure? [yes/no]")
    sure <- getLine
    if sure == "yes" then do
        putStrLn "Hit Enter on any column to leave it unchanged."
        temprow <- askForValues colnames
        let newrows = map (\row -> if testPredicate predicate db row then [if fst val /= "" then fst val else snd val | val <- zip temprow row] else row) rows
        saveDB db newrows
    else return ()

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
            'e' -> cmd_select dbname
            'f' -> cmd_delete dbname
            'g' -> cmd_update dbname
            _ -> putStrLn "Unknown command."
        main
    else return ()
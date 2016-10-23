module GameOfLife where
import Data.Array
import Data.Char
import Control.Monad

data Life = Alive | Dead deriving (Eq)
instance Show Life where
    show Alive = "O"
    show Dead = "-"
instance Read Life where
    readsPrec _ ('O':xs) = [(Alive, xs)]
    readsPrec _ ('-':xs) = [(Dead, xs)]

newtype Config = Config (Array (Int, Int) Life)
instance Show Config where
    show c = showConfig c

--test = Config (array ((0, 0), (2, 2)) [((0, 0), Dead), ((0, 1), Dead), ((0, 2), Alive), ((1, 0), Alive), ((1, 1), Dead), ((1, 2), Alive), ((2, 0), Dead), ((2, 1), Alive), ((2, 2), Alive)])
--test = Config (array ((0, 0), (2, 1)) [((0, 0), Alive), ((0, 1), Alive), ((1, 0), Dead), ((1, 1), Dead), ((2, 0), Alive), ((2, 1), Dead)])

showConfig :: Config -> String
showConfig (Config arr) = let (_, (_, edge)) = bounds arr in concat $ map (\(y, x) -> if x == edge then (show (arr ! (y, x)))++"\n" else show (arr ! (y, x))) $ indices arr

-- TODO Maybe some better checks here
parseSize :: String -> (Int, Int)
parseSize ('(':xs) = if last xs == ')' then parseSize $ init xs else error "Invalid numbers."
parseSize s = let w = words s in (read $ takeWhile isDigit (w !! 0) :: Int, read (w !! 1) :: Int)

parseLine :: Int -> String -> Config -> Config
parseLine line str cnf = _parseLine cnf line str 0

_parseLine :: Config -> Int -> String -> Int -> Config
_parseLine cnf _ "" _ = cnf
_parseLine (Config arr) line (c:str) pos = _parseLine (Config (arr // [((line, pos), (read [c] :: Life))])) line str (pos + 1)

runGeneration :: Config -> Config
runGeneration (Config arr) =
    let cellLife = cellLives (Config arr)
        idx = indices arr
        cellStatus = map cellLife idx
    in Config (array (bounds arr) $ zip idx cellStatus)

livingNeighbours :: Config -> (Int, Int) -> Int
livingNeighbours cfg (y, x) = length [(a, b) | a <- [y-1..y+1], b <- [x-1..x+1], (a, b) /= (y, x), cellIsAlive cfg (a, b)]

cellLives :: Config -> (Int, Int) -> Life
cellLives cfg pos =
    let living = livingNeighbours cfg pos
    in
        if (cellIsAlive cfg pos) && (living == 2 || living == 3) then Alive
        else if living == 3 then Alive
        else Dead

cellIsAlive :: Config -> (Int, Int) -> Bool
cellIsAlive (Config arr) (y, x) =
    let (_, (by, bx)) = bounds arr
    in
        if x < 0 ||  y < 0 || x > bx || y > by then False else ((arr ! (y, x)) == Alive)

main :: IO ()
main = do
    putStrLn "Enter your board size (width, height):"
    size <- getLine
    let (x, y) = parseSize size
    putStrLn "Enter your initial board:"
    lines <- replicateM y getLine
    let parseLines = foldr (.) id [parseLine i line | (i, line) <- zip [0..(length lines - 1)] lines]
        config = parseLines (Config (array ((0, 0), (y-1, x-1)) []))

    mainLoop config

mainLoop :: Config -> IO ()
mainLoop config = do
    putStrLn "Your current board:"
    putStr $ show config
    putStrLn "Press Q to quit or anything else to continue."
    char <- getChar
    if char == 'q' || char == 'Q'
        then return ()
        else mainLoop $ runGeneration config
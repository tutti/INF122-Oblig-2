module GameOfLife where
import Data.Array
import Data.Char
import Control.Monad

{-
    Navn: Pål Vårdal Gjerde

    Tilleggsopplysninger:
    Størrelse på brettet skal angis som to heltall. De skrives inn på samme
    linje, separert av mellomrom, og paret kan omsluttes av en parentes.
    Eventuelle andre tegn etter hvert tall vil kastes (derfor er f. eks
    "(10, 10)" lov).

    Eksempelbrett:
    Sett størrelse til 38 20
    [((5,1),Alive),((5,2),Alive),((6,1),Alive),((6,2),Alive),((3,14),Alive),((3,13),Alive),((4,12),Alive),((5,11),Alive),((6,11),Alive),((7,11),Alive),((8,12),Alive),((9,13),Alive),((9,14),Alive),((6,15),Alive),((4,16),Alive),((5,17),Alive),((6,17),Alive),((6,18),Alive),((7,17),Alive),((8,16),Alive),((2,23),Alive),((3,22),Alive),((3,21),Alive),((4,22),Alive),((4,21),Alive),((5,22),Alive),((5,21),Alive),((6,23),Alive),((1,25),Alive),((2,25),Alive),((6,25),Alive),((7,25),Alive),((3,35),Alive),((3,36),Alive),((4,35),Alive),((4,36),Alive)]
-}

data Life = Alive | Dead deriving (Eq)
instance Show Life where
    show Alive = "O"
    show Dead = "-"
instance Read Life where
    readsPrec _ ('O':xs) = [(Alive, xs)]
    readsPrec _ ('-':xs) = [(Dead, xs)]
    readsPrec _ s = 
        if take 5 s == "Alive" then [(Alive, drop 5 s)]
        else if take 4 s == "Dead" then [(Dead, drop 4 s)]
        else error ("Can't read Life from "++s)

newtype Config = Config (Array (Int, Int) Life)
instance Show Config where
    show c = showConfig c

showConfig :: Config -> String
showConfig (Config arr) = let (_, (_, edge)) = bounds arr in concat $ map (\(y, x) -> if x == edge then (show (arr ! (y, x)))++"\n" else show (arr ! (y, x))) $ indices arr

parseSize :: String -> (Int, Int)
parseSize ('(':xs) = if last xs == ')' then parseSize $ init xs else error "Invalid numbers."
parseSize s = let w = words s in (read $ takeWhile isDigit (w !! 0) :: Int, read (w !! 1) :: Int)

parseCells :: [((Int, Int), Life)] -> String -> [((Int, Int), Life)]
parseCells _ ('[':str) = parseCells [] str
parseCells prev ('(':'(':str) =
    let n1 = takeWhile isDigit str
        str1' = dropWhile isDigit str
        comma1 = head str1'
        str1 = dropWhile isSpace $ tail str1'
        n2 = takeWhile isDigit str1
        str2' = dropWhile isDigit str1
        paren1 = head str2'
        comma2 = str2' !! 1
        str2 = dropWhile isSpace $ drop 2 str2'
        [(life, str3')] = readsPrec 0 str2 :: [(Life, String)]
        paren2 = head str3'
        str3 = tail str3'
    in
        if
            comma1 /= ','
            || comma2 /= ','
            || paren1 /= ')'
            || paren2 /= ')'
        then
            error "Invalid board state."
        else
            let num1 = read n1 :: Int
                num2 = read n2 :: Int
            in
                parseCells (prev ++ [((num1, num2), life)]) (if head str3 == ',' then dropWhile isSpace $ tail str3 else str3)
parseCells prev (']':xs) = prev
parseCells _ s = error ("Invalid cell "++s)

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
    initial <- getLine
    let cells = parseCells [] initial
        emptyList = [((a, b), Dead) | a <- [0..y-1], b <- [0..x-1]]
        config = Config (array ((0, 0), (y-1, x-1)) (emptyList++cells) )
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

module Main where
import System.Environment
import Data.List
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
-- import Text.ParserCombinators.Parsec.Token
-- import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec ( Parsec )
import Control.Arrow (Arrow(second))
import Data.List.Split
import GHC.Arr
import Data.Set 

data Direction = Up | Right | Down | Left
    deriving (Eq, Show, Ord)
data MapElement = Empty | Antenna Char
    deriving (Eq, Show, Ord)

getMapElements :: [Char] -> ([MapElement])
getMapElements [] = []
getMapElements (x:xs) =
    element : getMapElements xs
    where
        element = case x of
            '.' -> Empty
            c -> Antenna c

getMap :: [[MapElement]] -> Array (Int, Int) MapElement
getMap mapList = array ((0, 0), (height - 1, width - 1)) ([((i,j), mapList!!i!!j) | i <- [0..height-1], j <- [0..width-1]])
    where
        height = fromIntegral $ length mapList 
        width = fromIntegral $ length (head mapList)

getAllAntennas :: Array (Int, Int) MapElement -> [(Int, Int, MapElement)]
getAllAntennas map = [(i, j, map!(i,j)) | i <- [0..h], j <-[0..w], (map!(i,j)) /= Empty ]
    where
        (_, (h, w)) = bounds map


getAntinodes :: Set (Int, Int) -> Int -> Int -> (Int, Int, MapElement) -> (Int, Int, MapElement)  -> Set (Int, Int)
getAntinodes antinodesSoFar h w (i1, j1, e1) (i2, j2, e2) =
    if e1 == e2 then
        antinodesSoFar2
    else
        antinodesSoFar
    where
        antinodesSoFar1 = if i1 + i1 - i2 >= 0 && i1 + i1 - i2 <= h  && j1 + j1 - j2 >= 0 && j1 + j1 - j2 <= w then
            Data.Set.insert (i1 + i1 - i2, j1 +j1-j2)  antinodesSoFar else antinodesSoFar
        antinodesSoFar2 = if i2 + i2 - i1 >= 0 && i2 + i2 - i1 <= h  && j2 + j2 - j1 >= 0 && j2 + j2 - j1 <= w then
            Data.Set.insert (i2 + i2 - i1, j2 +j2-j1)  antinodesSoFar1 else antinodesSoFar1

isInLine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isInLine (a1, a2) (b1, b2) (c1, c2) = dotproduct * dotproduct == dx2 * dy2
    where
        (x1, x2) = (a1-b1, a2-b2)
        (y1, y2) = (a1-c1, a2-c2)
        dx2 = x1*x1+x2*x2
        dy2 = y1*y1+y2*y2
        dotproduct = x1*y1+x2*y2   

getAntinodes2 :: Set (Int, Int) -> Int -> Int -> (Int, Int, MapElement) -> (Int, Int, MapElement)  -> Set (Int, Int)
getAntinodes2 antinodesSoFar h w (i1, j1, e1) (i2, j2, e2) =
    if e1 == e2 then
        antinodesSoFar2
    else
        antinodesSoFar
    where
        list = [(i, j) | i <- [0..h], j <- [0..w], isInLine (i1, j1) (i2, j2) (i, j)]
        antinodesSoFar2 = Data.List.foldr Data.Set.insert antinodesSoFar list

uniquePairs :: Ord b => [b] -> [(b, b)]
uniquePairs l = [(x,y) | x <- l, y <- l, x < y]

getAllAntinodes :: Set (Int, Int) -> Int -> Int -> [((Int, Int, MapElement), (Int, Int, MapElement))] -> Set (Int, Int) 
getAllAntinodes antinodesSoFar h w [] = antinodesSoFar
getAllAntinodes antinodesSoFar h w ((a,b):xs) = 
    getAllAntinodes (getAntinodes antinodesSoFar h w a b) h w xs

getAllAntinodes2 :: Set (Int, Int) -> Int -> Int -> [((Int, Int, MapElement), (Int, Int, MapElement))] -> Set (Int, Int) 
getAllAntinodes2 antinodesSoFar h w [] = antinodesSoFar
getAllAntinodes2 antinodesSoFar h w ((a,b):xs) = 
    getAllAntinodes2 (getAntinodes2 antinodesSoFar h w a b) h w xs


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let elementMap = getMap (Data.List.map getMapElements $ lines file)
    let antennas = getAllAntennas elementMap
    let (_, (h, w)) = bounds elementMap
    let antennaPairs = uniquePairs antennas
    let antinodes = getAllAntinodes Data.Set.empty h w antennaPairs
    return $ fromIntegral $ size antinodes

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let elementMap = getMap (Data.List.map getMapElements $ lines file)
    let antennas = getAllAntennas elementMap
    let (_, (h, w)) = bounds elementMap
    let antennaPairs = uniquePairs antennas
    let antinodes = getAllAntinodes2 Data.Set.empty h w antennaPairs
    return $ fromIntegral $ size antinodes



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

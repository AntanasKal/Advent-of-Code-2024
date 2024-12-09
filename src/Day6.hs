
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
data MapElement = Empty | Obstacle | Character Direction
    deriving (Eq, Show)

isCharacter :: MapElement -> Direction -> Bool
isCharacter (Character dir1) dir2 = dir1 == dir2
isCharacter _ _ = False





getMapElements :: [Char] -> ([MapElement])
getMapElements [] = []
getMapElements (x:xs) =
    element : getMapElements xs
    where
        element = case x of
            '.' -> Empty
            '#' -> Obstacle
            '^' -> Character Up
            'v' -> Character Down
            '>' -> Character Main.Right
            '<' -> Character Main.Left
            c -> error $ "bad character: "++[c]

getMap :: [[MapElement]] -> Array (Int, Int) MapElement
getMap mapList = array ((0, 0), (height - 1, width - 1)) ([((i,j), mapList!!i!!j) | i <- [0..height-1], j <- [0..width-1]])
    where
        height = fromIntegral $ length mapList 
        width = fromIntegral $ length (head mapList)

getInitialCharacterPosition :: Array (Int, Int) MapElement -> (Int, Int, Direction)
getInitialCharacterPosition arr =
    head positions
    where
        positions = [(i, j, dir) | i <- [0..h], j <-[0..w], dir <- [Up, Main.Left, Main.Right, Down], (arr!(i,j)) == Character dir]
        (_, (h, w)) = bounds arr

step :: Array (Int, Int) MapElement -> Set (Int, Int) -> (Int, Int, Direction) -> Set (Int, Int)
step map visited (i, 0, Main.Left) =
    Data.Set.insert (i, 0) visited
step map visited (i, j, Main.Left) =
    if map!(i,j-1) /= Obstacle then
        step map (Data.Set.insert (i, j) visited) (i, j-1, Main.Left)
    else
        step map (Data.Set.insert (i, j) visited) (i, j, Main.Up)
step map visited (0, j, Main.Up) =
    Data.Set.insert (0, j) visited
step map visited (i, j, Main.Up) =
    if map!(i-1,j) /= Obstacle then
        step map (Data.Set.insert (i, j) visited) (i-1, j, Main.Up)
    else
        step map (Data.Set.insert (i, j) visited) (i, j, Main.Right)
step map visited (i, j, Main.Right) =
    if j < w then
        if map!(i,j+1) /= Obstacle then
            step map (Data.Set.insert (i, j) visited) (i, j+1, Main.Right)
        else
            step map (Data.Set.insert (i, j) visited) (i, j, Main.Down)
    else
        Data.Set.insert (i, j) visited
    where
        (_, (h,w)) = bounds map
step map visited (i, j, Main.Down) =
    if i < h then
        if map!(i+1,j) /= Obstacle then
            step map (Data.Set.insert (i, j) visited) (i+1, j, Main.Down)
        else
            step map (Data.Set.insert (i, j) visited) (i, j, Main.Left)
    else
        Data.Set.insert (i, j) visited
    where
        (_, (h,w)) = bounds map



isLoop :: Array (Int, Int) MapElement -> Set (Int, Int, Direction) -> (Int, Int, Direction) -> Bool
isLoop map visited (i, 0, Main.Left) =
    False
isLoop map visited (i, j, Main.Left) =
    if map!(i,j-1) /= Obstacle then
        member (i, j-1, Main.Left) visited || isLoop map (Data.Set.insert (i, j, Main.Left) visited) (i, j-1, Main.Left)
    else
        isLoop map (Data.Set.insert (i, j, Main.Left) visited) (i, j, Main.Up)
isLoop map visited (0, j, Main.Up) =
    False
isLoop map visited (i, j, Main.Up) =
    if map!(i-1,j) /= Obstacle then
        member (i-1, j, Main.Up) visited || isLoop map (Data.Set.insert (i, j, Main.Up) visited) (i-1, j, Main.Up)
    else
        isLoop map (Data.Set.insert (i, j, Main.Up) visited) (i, j, Main.Right)
isLoop map visited (i, j, Main.Right) =
    if j < w then
        if map!(i,j+1) /= Obstacle then
            member (i, j+1, Main.Right) visited || isLoop map (Data.Set.insert (i, j, Main.Right) visited) (i, j+1, Main.Right)
        else
            isLoop map (Data.Set.insert (i, j, Main.Right) visited) (i, j, Main.Down)
    else
        False
    where
        (_, (h,w)) = bounds map
isLoop map visited (i, j, Main.Down) =
    if i < h then
        if map!(i+1,j) /= Obstacle then
            member (i+1, j, Main.Down) visited ||  isLoop map (Data.Set.insert (i, j, Main.Down) visited) (i+1, j, Main.Down)
        else
            isLoop map (Data.Set.insert (i, j, Main.Down) visited) (i, j, Main.Left)
    else
        False
    where
        (_, (h,w)) = bounds map

insertObstacle :: (Int, Int) -> Array (Int, Int) MapElement -> Array (Int, Int) MapElement
insertObstacle (i,j) map =
    map//[((i,j), Obstacle)]

solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let elementMap = getMap (Data.List.map getMapElements $ lines file)
    let initialPosition = getInitialCharacterPosition elementMap
    let visitedStates = step elementMap Data.Set.empty initialPosition
    return $ fromIntegral $ size visitedStates

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let elementMap = getMap (Data.List.map getMapElements $ lines file)
    let initialPosition@(init_i,init_j,_) = getInitialCharacterPosition elementMap
    let visitedStates = Data.Set.filter (\x -> x/= (init_i,init_j)) $ step elementMap Data.Set.empty initialPosition

    let loopedPositions = Data.Set.filter (\(i,j) -> (isLoop (insertObstacle (i,j) elementMap) empty initialPosition)) visitedStates
    return $ fromIntegral $ size loopedPositions


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

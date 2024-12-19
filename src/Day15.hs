
module Main where
import System.Environment
import Data.List
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Prim
-- import Text.ParserCombinators.Parsec.Token
-- import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec ( Parsec, endOfLine )
import Control.Arrow (Arrow(second))
import Data.List.Split
import Data.Either
import GHC.Arr
import Data.Maybe (fromMaybe)
import Data.Monoid (All(getAll))
-- import Data.Set 


data MapElement = Wall | Box | Character | Space
    deriving (Eq, Show, Ord)


data MapElement2 = Wall2 | BoxL | BoxR | Character2 | Space2
    deriving (Eq, Show, Ord)

data Direction = DUp | DDown | DLeft | DRight
    deriving (Eq, Show, Ord)


parseMapElement :: Char -> MapElement
parseMapElement '#' = Wall
parseMapElement 'O' = Box
parseMapElement '@' = Character
parseMapElement '.' = Space


parseMap :: String -> (Array (Integer, Integer) MapElement, (Integer, Integer))
parseMap input = (mapArray, initialPosition)
    where
        list = map (map parseMapElement) (lines input)
        h = fromIntegral $ length list - 1
        w = fromIntegral $ (length $ head list) - 1
        mapArray = array ((0,0), (h,w)) [((i,j), list!! fromIntegral i !! fromIntegral j) | i <- [0..h], j <- [0..w]]
        initialPosition = head $ [(i,j) | i <- [0..h], j <- [0..w], mapArray ! (i,j) == Character ]


parseDirection :: Char -> Direction
parseDirection '>' = DRight
parseDirection '<' = DLeft
parseDirection 'v' = DDown
parseDirection '^' = DUp

parseDirections :: String -> [Direction]
parseDirections str = parseDirection <$> str


moveLeft2 :: Array (Integer, Integer) MapElement2 -> (Integer, Integer) -> Array (Integer, Integer) MapElement2
moveLeft2 mapArray (i, j) =
    case mapArray ! (i', j') of
        Space2 -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
        Wall2 -> mapArray
        BoxL ->
            if movedMap ! (i',j') == Space2 then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
    where
        i' = i
        j' = j-1
        movedMap = (moveLeft2 mapArray (i', j'))


moveUp2 :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Array (Integer, Integer) MapElement
moveUp2 mapArray (i, j) =
    case mapArray ! (i', j') of
        Space -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
        Wall -> mapArray
        Box ->
            if movedMap ! (i',j') == Space then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
            else
                mapArray
    where
        i' = i-1
        j' = j
        movedMap = (moveUp mapArray (i', j'))



moveUp :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Array (Integer, Integer) MapElement
moveUp mapArray (i, j) =
    case mapArray ! (i', j') of
        Space -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
        Wall -> mapArray
        Box ->
            if movedMap ! (i',j') == Space then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
            else
                mapArray
    where
        i' = i-1
        j' = j
        movedMap = (moveUp mapArray (i', j'))



moveDown :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Array (Integer, Integer) MapElement
moveDown mapArray (i, j) =
    case mapArray ! (i', j') of
        Space -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
        Wall -> mapArray
        Box ->
            if movedMap ! (i',j') == Space then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
            else
                mapArray
    where
        i' = i+1
        j' = j
        movedMap = (moveDown mapArray (i', j'))

moveLeft :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Array (Integer, Integer) MapElement
moveLeft mapArray (i, j) =
    case mapArray ! (i', j') of
        Space -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
        Wall -> mapArray
        Box ->
            if movedMap ! (i',j') == Space then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
            else
                mapArray
    where
        i' = i
        j' = j-1
        movedMap = (moveLeft mapArray (i', j'))

moveRight :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Array (Integer, Integer) MapElement
moveRight mapArray (i, j) =
    case mapArray ! (i', j') of
        Space -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
        Wall -> mapArray
        Box ->
            if movedMap ! (i',j') == Space then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space)]
            else
                mapArray
    where
        i' = i
        j' = j+1
        movedMap = (moveRight mapArray (i', j'))


move :: (Array (Integer, Integer) MapElement, (Integer, Integer)) -> Direction -> (Array (Integer, Integer) MapElement, (Integer, Integer))
move (mapArray, (i,j)) DUp = (moveUp mapArray (i,j), if moveUp mapArray (i,j) ! (i,j) == Character then (i,j) else (i-1,j))
move (mapArray, (i,j)) DDown = (moveDown mapArray (i,j), if moveDown mapArray (i,j) ! (i,j) == Character then (i,j) else (i+1,j))
move (mapArray, (i,j)) DLeft = (moveLeft mapArray (i,j), if moveLeft mapArray (i,j) ! (i,j) == Character then (i,j) else (i,j-1))
move (mapArray, (i,j)) DRight = (moveRight mapArray (i,j), if moveRight mapArray (i,j) ! (i,j) == Character then (i,j) else (i,j+1))


findCharacter :: Array (Integer, Integer) MapElement -> (Integer, Integer)
findCharacter mapArray = head [(i,j) | i <- [0..h], j<-[0..w], mapArray ! (i,j) == Character]
    where
        (_, (h, w)) = bounds mapArray



getFinalNumber :: Array (Integer, Integer) MapElement -> Integer
getFinalNumber mapArray =  sum [100*i + j | i <- [0..h], j<-[0..w], mapArray ! (i,j) == Box]
    where
        (_, (h, w)) = bounds mapArray


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let (mapArray, initialCharPos) = parseMap $ head (splitOn "\n\n" file)
    let directions = parseDirections $ concat $ lines $ splitOn "\n\n" file !! 1
    print directions
    print $ mapArray
    -- print $ move (mapArray, initialCharPos) DLeft 
    let (finalMap, pos) = foldl move (mapArray, initialCharPos) directions
    print $ getFinalNumber finalMap

    -- print directions
    return 1

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    return 2

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

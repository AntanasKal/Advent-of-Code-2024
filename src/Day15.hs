
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

convertList :: [[MapElement]] -> [[MapElement2]]
convertList = map convertLine
    where
        convertLine line = concat (convertElement <$> line)
        convertElement element =
            case element of
                Space -> [Space2, Space2]
                Wall -> [Wall2, Wall2]
                Box -> [BoxL, BoxR]
                Character -> [Character2, Space2]



parseMap2 :: String -> (Array (Integer, Integer) MapElement2, (Integer, Integer))
parseMap2 input = (mapArray, initialPosition)
    where
        list = convertList $ map (map parseMapElement) (lines input)
        h = fromIntegral $ length list - 1
        w = fromIntegral $ (length $ head list) - 1
        mapArray = array ((0,0), (h,w)) [((i,j), list!! fromIntegral i !! fromIntegral j) | i <- [0..h], j <- [0..w]]
        initialPosition = head $ [(i,j) | i <- [0..h], j <- [0..w], mapArray ! (i,j) == Character2 ]



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
        _ ->
            if movedMap ! (i',j') == Space2 then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
    where
        i' = i
        j' = j-1
        movedMap = (moveLeft2 mapArray (i', j'))


moveUp2 :: Array (Integer, Integer) MapElement2 -> (Integer, Integer) -> Array (Integer, Integer) MapElement2
moveUp2 mapArray (i, j) =
    case mapArray ! (i', j') of
        Space2 -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
        Wall2 -> mapArray
        BoxL ->
            if movedMap1 ! (i',j') == Space2 && movedMap2L ! (i',j'+1) == Space2  then
                movedMap2L // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
        BoxR ->
            if movedMap1 ! (i',j') == Space2 && movedMap2R ! (i',j'-1) == Space2  then
                movedMap2R // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
    where
        i' = i-1
        j' = j
        movedMap1 = moveUp2 mapArray (i', j')
        movedMap2L = moveUp2 movedMap1 (i', j'+1)
        movedMap2R = moveUp2 movedMap1 (i', j'-1)


moveDown2 :: Array (Integer, Integer) MapElement2 -> (Integer, Integer) -> Array (Integer, Integer) MapElement2
moveDown2 mapArray (i, j) =
    case mapArray ! (i', j') of
        Space2 -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
        Wall2 -> mapArray
        BoxL ->
            if movedMap1 ! (i',j') == Space2 && movedMap2L ! (i',j'+1) == Space2  then
                movedMap2L // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
        BoxR ->
            if movedMap1 ! (i',j') == Space2 && movedMap2R ! (i',j'-1) == Space2  then
                movedMap2R // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
    where
        i' = i+1
        j' = j
        movedMap1 = moveDown2 mapArray (i', j')
        movedMap2L = moveDown2 movedMap1 (i', j'+1)
        movedMap2R = moveDown2 movedMap1 (i', j'-1)


moveRight2 :: Array (Integer, Integer) MapElement2 -> (Integer, Integer) -> Array (Integer, Integer) MapElement2
moveRight2 mapArray (i, j) =
    case mapArray ! (i', j') of
        Space2 -> mapArray // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
        Wall2 -> mapArray
        _ ->
            if movedMap ! (i',j') == Space2 then
                movedMap // [((i',j'), mapArray ! (i,j)), ((i,j), Space2)]
            else
                mapArray
    where
        i' = i
        j' = j+1
        movedMap = (moveRight2 mapArray (i', j'))


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



move2 :: (Array (Integer, Integer) MapElement2, (Integer, Integer)) -> Direction -> (Array (Integer, Integer) MapElement2, (Integer, Integer))
move2 (mapArray, (i,j)) DUp = (moveUp2 mapArray (i,j), if moveUp2 mapArray (i,j) ! (i,j) == Character2 then (i,j) else (i-1,j))
move2 (mapArray, (i,j)) DDown = (moveDown2 mapArray (i,j), if moveDown2 mapArray (i,j) ! (i,j) == Character2 then (i,j) else (i+1,j))
move2 (mapArray, (i,j)) DLeft = (moveLeft2 mapArray (i,j), if moveLeft2 mapArray (i,j) ! (i,j) == Character2 then (i,j) else (i,j-1))
move2 (mapArray, (i,j)) DRight = (moveRight2 mapArray (i,j), if moveRight2 mapArray (i,j) ! (i,j) == Character2 then (i,j) else (i,j+1))


findCharacter :: Array (Integer, Integer) MapElement -> (Integer, Integer)
findCharacter mapArray = head [(i,j) | i <- [0..h], j<-[0..w], mapArray ! (i,j) == Character]
    where
        (_, (h, w)) = bounds mapArray



getFinalNumber :: Array (Integer, Integer) MapElement -> Integer
getFinalNumber mapArray =  sum [100*i + j | i <- [0..h], j<-[0..w], mapArray ! (i,j) == Box]
    where
        (_, (h, w)) = bounds mapArray

getFinalNumber2 :: Array (Integer, Integer) MapElement2 -> Integer
getFinalNumber2 mapArray =  sum [100*i + j | i <- [0..h], j<-[0..w], mapArray ! (i,j) == BoxL]
    where
        (_, (h, w)) = bounds mapArray


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let (mapArray, initialCharPos) = parseMap $ head (splitOn "\n\n" file)
    let directions = parseDirections $ concat $ lines $ splitOn "\n\n" file !! 1
    let (finalMap, pos) = foldl move (mapArray, initialCharPos) directions
    return $ getFinalNumber finalMap


displayMap :: Array (Integer, Integer) MapElement2 -> String
displayMap mapArray = unlines  [ [elementToChar (mapArray ! (i,j)) | j <- [0..w]] | i <- [0..h]]
    where
        (_, (h, w)) = bounds mapArray
        elementToChar c = case c of
            BoxL -> '['
            BoxR -> ']'
            Space2 -> '.'
            Character2 -> '@'
            Wall2 -> '#'


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let (mapArray, initialCharPos) = parseMap2 $ head (splitOn "\n\n" file)
    let directions = parseDirections $ concat $ lines $ splitOn "\n\n" file !! 1
    print directions
    print $ mapArray
    -- print $ move (mapArray, initialCharPos) DLeft 
    let (finalMap, pos) = foldl move2 (mapArray, initialCharPos) directions
    print finalMap
    -- putStrLn $ displayMap finalMap
    -- print $ getFinalNumber2 finalMap

    return $ getFinalNumber2 finalMap

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

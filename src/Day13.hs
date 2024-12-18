
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
import Data.Ratio


newtype MapElement = Height Integer
    deriving (Eq, Show, Ord)

oneHigher :: MapElement -> MapElement 
oneHigher (Height x) = Height $ x+1

parseHeight :: Char -> MapElement
parseHeight '.' = Height 100
parseHeight c = Height $ read [c]



parseTwoDigits :: Parsec String () (Integer, Integer)
parseTwoDigits = do
    x <- digit
    y <- digit
    pure (read (show x), read (show y))

parseInput' :: Parsec String () [(Integer, Integer)]
parseInput' = many parseTwoDigits <* eof


parseSingleInput :: String -> ((Integer, Integer), (Integer, Integer), (Integer, Integer))
parseSingleInput str =
        ((ax,ay), (bx,by), (px,py))
        -- ((ax,ay), (bx,by), (px,py))
    where
        allLines = lines str
        ax = read $ (splitOn "," ((splitOn "+" ((splitOn " " (allLines!!0))!!2))!!1)!!0)
        ay = read $ (splitOn "," ((splitOn "+" ((splitOn " " (allLines!!0))!!3))!!1)!!0)
        bx = read $ (splitOn "," ((splitOn "+" ((splitOn " " (allLines!!1))!!2))!!1)!!0)
        by = read $ (splitOn "," ((splitOn "+" ((splitOn " " (allLines!!1))!!3))!!1)!!0)
        px = read $ (splitOn "," ((splitOn "=" ((splitOn " " (allLines!!2))!!1))!!1))!!0
        py = read $ (splitOn "," ((splitOn "=" ((splitOn " " (allLines!!2))!!2))!!1))!!0
        -- py = read $ (splitOn "=" ((splitOn " " (allLines!!2))!!1))!!1


parseInput :: String -> (Array (Integer, Integer) MapElement, Array (Integer, Integer) Bool)
parseInput input = (mapArray, visitedArray)
    where
        list = map (map parseHeight) (lines input)
        h = fromIntegral $ length list - 1
        w = fromIntegral $ (length $ head list) - 1
        mapArray = array ((0,0), (h,w)) [((i,j), list!! fromIntegral i !! fromIntegral j) | i <- [0..h], j <- [0..w]]
        visitedArray = array ((0,0), (h,w)) [((i,j), False) | i <- [0..h], j <- [0..w]]


doSearch :: Array (Integer, Integer) MapElement -> [(Integer, Integer)] -> Array (Integer, Integer) Bool -> Array (Integer, Integer) Bool
doSearch map [] visited = visited
doSearch map ((x@(x1,x2)):xs) visited = doSearch map (neighbours++xs) visited'
    where
        visited' = visited // [(x,True)]
        neighbours = [(y1,y2)| 
            y@(y1, y2) <- [(x1-1,x2), (x1+1, x2), (x1, x2-1), (x1, x2+1)]
            , y1 >= 0
            , y1 <= h
            , y2 >= 0
            , y2 <= w
            , not $ visited'!y
            , map!y == oneHigher (map!x)
             ]
        (_, (h, w)) = bounds map


countPaths :: Array (Integer, Integer) MapElement -> (Integer, Integer) -> Integer
countPaths heightMap x@(x1,x2) =
    case
        heightMap!x of
            Height 100 -> 0
            Height 9 -> 1
            _ -> sum $ map (countPaths heightMap) neighbours
    where
        neighbours = [(y1,y2)| 
            y@(y1, y2) <- [(x1-1,x2), (x1+1, x2), (x1, x2-1), (x1, x2+1)]
            , y1 >= 0
            , y1 <= h
            , y2 >= 0
            , y2 <= w
            , heightMap!y == oneHigher (heightMap!x)
             ]
        (_, (h, w)) = bounds heightMap


getAllZeros :: Array (Integer, Integer) MapElement -> [(Integer, Integer)]
getAllZeros map = [(i,j) | i <- [0..h], j<- [0..w], map!(i,j)== Height 0]
    where
        (_, (h, w)) = bounds map

findDegree :: Array (Integer, Integer) MapElement -> Array (Integer, Integer) Bool -> (Integer, Integer) -> Integer
findDegree map emptyVisited x = visitedNines
    where
        appliedSearch = doSearch map [x] emptyVisited
        visitedNines = fromIntegral $ length $ [(i,j)| i<-[0..h], j<-[0..w], appliedSearch!(i,j) == True, map!(i,j)== Height 9]
        (_, (h, w)) = bounds map

minHelper :: Integer -> [Integer] -> Integer
minHelper minSoFar xs = foldl min minSoFar xs

minOrZero :: [Integer] -> Integer
minOrZero [] = 0
minOrZero (x:xs) = minHelper x xs


solveForSingleInput :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Integer
solveForSingleInput ((ax,ay),(bx,by),(px,py)) = minOrZero [3*i+j |
      i <- [0..100]
    , j <- [0..100]
    , ax*i + bx*j == px
    , ay*i + by*j == py
    ]

inputToRational ((ax,ay),(bx,by),(px,py)) = ((toRational ax, toRational ay),(toRational bx, toRational by),(toRational px, toRational py)) 

solveForSingleInput2 :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Integer
solveForSingleInput2 ((ax,ay),(bx,by),(px',py')) =
    if denominator solutionA == 1 && denominator solutionB ==1 && numerator solutionA >= 0 && numerator solutionB >= 0 then
        3 * (numerator solutionA) + (numerator solutionB)
    else
        0
    where
        px = px'+10000000000000
        py = py'+10000000000000
        determinant = toRational $ ax * by - ay * bx
        solutionA = toRational (px*by - py*bx) / determinant
        solutionB = toRational (-(px*ay) + py*ax) / determinant


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let inputStrs = splitOn "\n\n" file
    let inputs = map parseSingleInput inputStrs
    return $ sum $ map solveForSingleInput inputs


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let inputStrs = splitOn "\n\n" file
    let inputs = map parseSingleInput inputStrs
    -- print $ map (\(a, b, p) -> (fst a)*(snd b)-(snd a)*(fst b)) inputs
    -- print $ map ((\((ax, ay), (bx, by), (px,py)) -> (px*by - py*bx) / toRational (ax*by-ay*bx)) . inputToRational) inputs
    return $ sum $ map solveForSingleInput2 inputs


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

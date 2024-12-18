
module Main where
import System.Environment
import Data.List
import Text.Parsec ( Parsec, endOfLine )
import Control.Arrow (Arrow(second))
import Data.List.Split
import Data.Either
import GHC.Arr
import Data.Maybe (fromMaybe)
import Control.Monad.State
import Data.List.Split.Internals (Splitter(finalBlankPolicy))
-- import Data.Map


parseSingleLine :: String -> ((Integer, Integer), (Integer, Integer))
parseSingleLine line = ((p1,p2),(v1,v2))
    where
        pStr = splitOn "=" (head $ splitOn " " line) !! 1
        vStr = splitOn "=" (splitOn " " line !! 1) !! 1
        p1 = read $ splitOn "," pStr !! 0
        p2 = read $ splitOn "," pStr !! 1
        v1 = read $ splitOn "," vStr !! 0
        v2 = read $ splitOn "," vStr !! 1

parseInput :: String -> [((Integer, Integer), (Integer, Integer))]
parseInput str = map parseSingleLine (lines str)


simulatePart1 :: Integer -> ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
simulatePart1 steps ((p1,p2), (v1,v2)) = (p1',p2')
    where
        w = 101
        h = 103
        p1' = (p1 + v1*steps) `mod` w
        p2' = (p2 + v2*steps) `mod` h

simulateAllPart1 :: [((Integer, Integer), (Integer, Integer))] -> Integer -> Integer
simulateAllPart1 list steps = q1 * q2 * q3 * q4
    where
        finalPositions = map (simulatePart1 steps) list
        q1 = fromIntegral $ length $ filter (\(p1,p2) -> p1 < w `div`2 && p2 < h `div`2 ) finalPositions
        q2 = fromIntegral $ length $ filter (\(p1,p2) -> p1 < w `div`2 && p2 > h `div`2  ) finalPositions
        q3 = fromIntegral $ length $ filter (\(p1,p2) -> p1 > w `div`2 && p2 < h `div`2 ) finalPositions
        q4 = fromIntegral $ length $ filter (\(p1,p2) -> p1 > w `div`2 && p2 > h `div`2  ) finalPositions
        w = 101
        h = 103



simulatePart2 :: Integer -> ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
simulatePart2 steps ((p1,p2), (v1,v2)) = (p1',p2')
    where
        w = 101
        h = 103
        p1' = (p1 + v1*steps) `mod` w
        p2' = (p2 + v2*steps) `mod` h


-- simulateAllPart2 :: Integer -> [((Integer, Integer), (Integer, Integer))]  -> Array (Integer, Integer) Char
-- simulateAllPart2 steps list = array ((0,0), ((h-1),(w-1))) [((i,j), if (j,i) `elem` finalPositions then 'O' else '.') | j<-[0..(w-1)], i <- [0..(h-1)]]
--     where
--         finalPositions = map simulatePart1 list
--         w = 101
--         h = 103

simulateAllPart2' :: [((Integer, Integer), (Integer, Integer))] -> Integer -> [[Char]]
simulateAllPart2' list steps = [ [if (j,i) `elem` finalPositions then 'O' else '.' | j <- [0..(w-1)] ]| i <- [0..(h-1)]]
    where
        finalPositions = map (simulatePart2 steps) list
        w = 101
        h = 103


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let input = parseInput file
    pure $ simulateAllPart1 input 100
    -- pure theRes

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let input = parseInput file
    mapM_ (action input) [x | x <- [0..11000]]
    -- let stuff = unlines $ simulateAllPart2' input 10
    -- putStrLn stuff
    pure 2
    where
        action input num = do
            -- putStrLn $ show $ simulateAllPart1 input num
            let finalString = unlines $ simulateAllPart2' input num
            if anotherPredicate finalString && bigPredicate finalString then
                do
                    putStrLn $ show num
                    putStrLn finalString
            else
                pure ()
        bigPredicate str = isInfixOf "OOOOOOOO" str
            -- all (\l -> isInfixOf (".O"++replicate l '.' ++ "O.") str) [2*i+1 | i <-[1..25]]
        anotherPredicate str = length [line | line <- lines str, isPrefixOf "............" line] > 50

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

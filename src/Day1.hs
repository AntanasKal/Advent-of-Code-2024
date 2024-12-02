module Main where

import Data.List 
import System.Environment
solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let wordStuff = map (words) (lines file)
    let firstColumn = sort $ map ((read :: String -> Integer) . (!! 0)) wordStuff
    let secondColumn = sort $ map ((read :: String -> Integer) . (!! 1)) wordStuff
    let res = sum $ map (\(a,b) -> abs (a-b)) (zip firstColumn secondColumn)
    return res

similarityScore :: [Integer] -> [Integer] -> Integer
similarityScore [] ys = 0
similarityScore (x:xs) ys = (toInteger $ length (filter (==x) ys)) * x + similarityScore xs ys 

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let wordStuff = map words (lines file)
    let firstColumn = sort $ map ((read :: String -> Integer) . (!! 0)) wordStuff
    let secondColumn = sort $ map ((read :: String -> Integer) . (!! 1)) wordStuff
    let res = similarityScore firstColumn secondColumn
    return res

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."






module Main where
import System.Environment
import Data.List 


isSafeIncreasing :: [Integer] -> Bool
isSafeIncreasing [] = True
isSafeIncreasing [x] = True
isSafeIncreasing (x1:x2:xs) = (x1 < x2) && (abs (x2 - x1) <= 3) && isSafeIncreasing (x2:xs)

isSafeDecreasing :: [Integer] -> Bool
isSafeDecreasing [] = True
isSafeDecreasing [x] = True
isSafeDecreasing (x1:x2:xs) = (x1 > x2) && (abs (x2 - x1) <= 3) && isSafeDecreasing (x2:xs)

isSafe :: [Integer] -> Bool
isSafe xs = isSafeIncreasing xs || isSafeDecreasing xs


isSafeIncreasing2 :: [Integer] -> Bool
isSafeIncreasing2 [] = True
isSafeIncreasing2 [x] = True
isSafeIncreasing2 (x1:x2:[]) = True
isSafeIncreasing2 (x1:x2:x3:xs) =
    if x1 < x2 && x2 < x3 && abs (x1-x2) <= 3 && abs (x2-x3) <= 3 then
        isSafeIncreasing2 (x2:x3:xs)
    else
        isSafeIncreasing (x1:x3:xs) || isSafeIncreasing (x1:x2:xs) || isSafeIncreasing (x2:x3:xs)


isSafeDecreasing2 :: [Integer] -> Bool
isSafeDecreasing2 [] = True
isSafeDecreasing2 [x] = True
isSafeDecreasing2 (x1:x2:[]) = True
isSafeDecreasing2 (x1:x2:x3:xs) =
    if x1 > x2 && x2 > x3 && abs (x1-x2) <= 3 && abs (x2-x3) <= 3 then
        isSafeDecreasing2 (x2:x3:xs)
    else
        isSafeDecreasing (x1:x3:xs) || isSafeDecreasing (x1:x2:xs) || isSafeDecreasing (x2:x3:xs)


isSafe2 :: [Integer] -> Bool
isSafe2 xs = isSafeIncreasing2 xs || isSafeDecreasing2 xs


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let sequences = map (map (read :: String -> Integer) . words) (lines file)
    let res = toInteger  $ length $ filter (==True) $ map isSafe sequences
    return res


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let sequences = map (map (read :: String -> Integer) . words) (lines file)
    let res = toInteger  $ length $ filter (==True) $ map isSafe2 sequences
    return res

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."
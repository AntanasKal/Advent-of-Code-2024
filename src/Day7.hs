
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
import GHC.Arr ()


concatIntegers :: Integer -> Integer -> Integer
concatIntegers a b = read $ show a ++ show b

parseLine :: String -> (Integer, [Integer])
parseLine line = (res, nums)
    where
        res = read $ head $ splitOn ": " line
        nums = map read $ splitOn " " (splitOn ": " line !!1)


canBeObtainedHelper :: Integer -> [Integer] -> Integer -> Bool
canBeObtainedHelper neededValue [] acc = neededValue == acc  
canBeObtainedHelper neededValue (x:xs) acc =
    canBeObtainedHelper neededValue xs (acc+x) || canBeObtainedHelper neededValue xs (acc*x)

canBeObtained :: Integer -> [Integer] -> Bool
canBeObtained neededValue [] = neededValue == 0
canBeObtained neededValue (x:xs) = canBeObtainedHelper neededValue xs x


canBeObtainedHelper2 :: Integer -> [Integer] -> Integer -> Bool
canBeObtainedHelper2 neededValue [] acc = neededValue == acc  
canBeObtainedHelper2 neededValue (x:xs) acc =
        canBeObtainedHelper2 neededValue xs (acc+x)
    || canBeObtainedHelper2 neededValue xs (acc*x)
    || canBeObtainedHelper2 neededValue xs (concatIntegers acc x)

canBeObtained2 :: Integer -> [Integer] -> Bool
canBeObtained2 neededValue [] = neededValue == 0
canBeObtained2 neededValue (x:xs) = canBeObtainedHelper2 neededValue xs x

solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let parsedLines = map parseLine (lines file)
    let obtainableResults = filter (uncurry canBeObtained) parsedLines
    return $ fromIntegral $ sum $ map fst obtainableResults

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let parsedLines = map parseLine (lines file)
    let obtainableResults = filter (uncurry canBeObtained2) parsedLines
    return $ fromIntegral $ sum $ map fst obtainableResults

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

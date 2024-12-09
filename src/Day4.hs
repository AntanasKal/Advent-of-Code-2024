
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

data Letter = X | M | A | S

countXMAS :: [Char] -> Integer
countXMAS [] = 0
countXMAS ('X':'M':'A':'S':xs) = 1 + countXMAS xs
countXMAS (x:xs) = countXMAS xs


getADiag ::[[a]] -> Int -> [a]
getADiag [] i = []
getADiag (x:xs) i =
    if i < length x then
        (x!!i):getADiag xs (i+1)
    else
        []

doubleMAS :: [[Char]] -> Int -> Int -> Bool
doubleMAS table i j =
    if i > 0 && i < length table - 1 then
        if j > 0 && j < length (table!!(i-1)) - 1 && j < length (table!!(i+1)) - 1 then
            (masOrSam first) && (masOrSam second)
        else
            False 
    else
        False

    where
        first = [table!!(i-1)!!(j-1), table!!i!!j, table!!(i+1)!!(j+1)]
        second = [table!!(i+1)!!(j-1), table!!i!!j, table!!(i-1)!!(j+1)]
        masOrSam = \x -> (x == ['M', 'A', 'S'] || x == ['S', 'A', 'M'])

getAllDiags :: [[a]] -> [[a]]
getAllDiags table =
        diags1 ++ (reverse <$> diags1)
    ++ diags2 ++ (reverse <$> diags2)
    where
        diags1 = map (getADiag table) [0..(length table)]
            ++ map (getADiag (transpose table)) [1..(length (transpose table))]
        diags2 = map (getADiag (reverse table)) [0..(length (reverse table))]
            ++ map (getADiag (transpose (reverse table))) [1..(length (transpose (reverse table)))]

getMASorSAM :: [[Char]] -> Int
getMASorSAM table = res2
    where
        x = zip ([0..] :: [Int]) table
        res1 = concat $ map (\(i, line) -> (map (\j -> (i,j)) [0..(length line)])) x
        res2 = length $ filter (uncurry (doubleMAS table)) res1


getAllHorizontalsVerticals :: [[a]] -> [[a]]
getAllHorizontalsVerticals table =
        table
    ++ (transpose table)
    ++ (reverse <$> table)
    ++ (reverse <$> transpose table)

getAllLines :: [[a]] -> [[a]]
getAllLines table = (getAllDiags table) ++ (getAllHorizontalsVerticals table)

solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let parseResult = parse parseTuples "" file
    let res = sum $ map countXMAS (getAllLines (lines file))
    return res


getEnabledParts :: String -> [String]
getEnabledParts str = map (head . splitOn "don't()") (splitOn "do()" str)


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    return $ fromIntegral $ getMASorSAM (lines file)


parseLine :: String -> [Integer]
parseLine x = []


integerParser :: Parsec String () Integer
integerParser = do
    x <- many digit
    let number = read x :: Integer
    return number

parseTuple :: Parsec String () (Integer, Integer)
parseTuple = do
    string "mul"
    char '('
    first <- integerParser
    char ','
    second <- integerParser
    char ')'
    return (first, second)

untilFound :: Parsec String () (Integer, Integer)
untilFound = do try parseTuple <|> (anyChar >> untilFound)


parseTuples :: Parsec String () [(Integer, Integer)]
parseTuples = many (try untilFound)



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

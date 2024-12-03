
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

solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let parseResult = parse parseTuples "" file
    let tupleList = mapM (parse parseTuples "")  (lines file)
    case tupleList of
        Left msg -> do
            print msg
            return 0
        Right list -> do
            let res = sum $ map (uncurry (*)) (concat list) 
            return res


getEnabledParts :: String -> [String]
getEnabledParts str = map (head . splitOn "don't()") (splitOn "do()" str)


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let enabledParts = getEnabledParts file
    let tupleList = mapM (parse parseTuples "") enabledParts
    case tupleList of
        Left msg -> do
            print msg
            return 0
        Right list -> do
            let res = sum $ map (uncurry (*)) (concat list) 
            return res


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

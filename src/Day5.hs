
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



parseLine :: String -> [Integer]
parseLine x = []


integerParser :: Parsec String () Integer
integerParser = do
    x <- many digit
    let number = read x :: Integer
    return number

-- parseTuple :: Parsec String () (Integer, Integer)
-- parseTuple = do
--     string "mul"
--     char '('
--     first <- integerParser
--     char ','
--     second <- integerParser
--     char ')'
--     return (first, second)

-- untilFound :: Parsec String () (Integer, Integer)
-- untilFound = do try parseTuple <|> (anyChar >> untilFound)


-- parseTuples :: Parsec String () [(Integer, Integer)]
-- parseTuples = many (try untilFound)


--  :: Parsec String () [(Integer, Integer)]


parseOrdering :: Parsec String () (Integer, Integer)
parseOrdering = do
    x <- many1 digit
    char '|'
    y <- many1 digit
    pure (read x :: Integer, read y :: Integer)

isOrderingSatisfied :: [Integer] -> (Integer, Integer) -> Bool
isOrderingSatisfied [] (a,b)  = True
isOrderingSatisfied (x:xs) (a,b) =
    if x==b then
        notElem a xs && isOrderingSatisfied xs (a, b)
    else
        isOrderingSatisfied xs (a, b)


mustPreceed :: [(Integer, Integer)] -> Integer -> Integer -> Bool
mustPreceed ords a b = (a,b) `elem` ords

doSort :: [(Integer, Integer)] -> [Integer] -> [Integer]
doSort ords [] = []
doSort ords (x:xs) = doSort ords ([x' | x' <- xs, mustPreceed ords x' x ]) ++ [x] ++ (doSort ords ([x' | x' <- xs, not (mustPreceed ords x' x) ]) )



allOrderingsSatisfied :: [Integer] -> [(Integer, Integer)]  -> Bool
allOrderingsSatisfied sequence = all (isOrderingSatisfied sequence)

countViolatedOrderings :: [Integer] -> [(Integer, Integer)] -> Int
countViolatedOrderings sequence ords = length $ filter (isOrderingSatisfied sequence) ords

getMiddleElem :: [Integer] -> Integer
getMiddleElem [] = -1000000
getMiddleElem [x] = x
getMiddleElem (x:y:[]) = -20000000
getMiddleElem xs = getMiddleElem (drop 1$ take (length xs -1) xs) 


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let stuff = splitOn ("\n\n") file
    let orderings = mapM (parse parseOrdering "") (lines $ head stuff)
    let sequences = map (map (read :: [Char] -> Integer) . splitOn ",") (lines $ stuff!!1)
    case orderings of
        Left msg -> do
            print msg
            return 1
        Right ords -> do
            let goodSequences = filter (\seq -> allOrderingsSatisfied seq ords) sequences
            let midElems = map getMiddleElem goodSequences
            return (sum midElems)


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let stuff = splitOn ("\n\n") file
    let orderings = mapM (parse parseOrdering "") (lines $ head stuff)
    let sequences = map (map (read :: [Char] -> Integer) . splitOn ",") (lines $ stuff!!1)
    case orderings of
        Left msg -> do
            print msg
            return 1
        Right ords -> do
            let badSeqences = filter (\seq -> not (allOrderingsSatisfied seq ords)) sequences
            let sortedBadSequences = map (doSort ords) badSeqences
            let midElems = map getMiddleElem sortedBadSequences
            return (sum midElems)



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

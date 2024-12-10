
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
-- import Data.Set 


parseTwoDigits :: Parsec String () (Integer, Integer)
parseTwoDigits = do
    x <- digit
    y <- digit
    pure (read (show x), read (show y))

parseInput' :: Parsec String () [(Integer, Integer)]
parseInput' = many parseTwoDigits <* eof


parseInput :: String -> [Integer]
parseInput = map (\ x -> read [x])


expandInput2 :: Integer -> [Integer] -> [Either Integer (Integer, Integer)]
expandInput2 index (f:e:ms) =
    Right (f,index):Left e:expandInput2 (index + 1) ms
expandInput2 index [f] =
    [Right (f,index)]
expandInput2 index [] = []

expandInput2Again :: [Either Integer (Integer, Integer)]-> [Integer]
expandInput2Again (Left k:ms) =
    replicate (fromIntegral k) 0 ++ expandInput2Again ms
expandInput2Again (Right (fileLength,fileIndex):ms) =
    replicate (fromIntegral fileLength) fileIndex ++ expandInput2Again ms
expandInput2Again [] = []


expandInput :: Integer -> [Integer] -> [(Maybe Integer)]
expandInput index (f:e:ms) =
        replicate (fromIntegral f) (Just index)
    ++ (replicate (fromIntegral e) Nothing)
    ++ expandInput (index+1) ms
expandInput index [f] =
        replicate (fromIntegral f) (Just index)
expandInput _ [] = []

findLastHelper :: Integer -> Maybe (Integer, a) -> [a] -> (a -> Bool) -> Maybe (Integer, a)
findLastHelper idx res [] p = res
findLastHelper idx ansSoFar (x:xs) p =
    if p x then
        findLastHelper (idx + 1) (Just (idx, x)) xs p
    else
        findLastHelper (idx + 1) ansSoFar xs p

findLast :: [a] -> (a -> Bool) -> Maybe (Integer, a)
findLast = findLastHelper 0 Nothing


findFirstHelper :: Integer -> [a] -> (a -> Bool) -> Maybe (Integer, a)
findFirstHelper idx [] p = Nothing
findFirstHelper idx (x:xs) p =
    if p x then
        (Just (idx, x))
    else
        findFirstHelper (idx + 1) xs p


findFirst :: [a] -> (a -> Bool) -> Maybe (Integer, a)
findFirst = findFirstHelper 0

compactify :: [Maybe Integer] -> [Maybe Integer]
compactify [] = []
compactify ((Just x):xs) = Just x : compactify xs
compactify (Nothing:xs) =
    case findLast xs (/= Nothing) of
        Nothing -> []
        Just (idx, val) -> val : compactify (take (fromIntegral idx) xs)




compactify2 :: Integer -> [Either Integer (Integer, Integer)] -> [Either Integer (Integer, Integer)]
compactify2 largestValue [] = []
compactify2 (- 1) xs = xs
compactify2 largestValue ys =
    case findLast xs' (predIsRight largestValue) of
        Nothing -> compactify2 (largestValue - 1) xs'
        (Just (idx, Right (fileLength, fileIdx))) ->
            case findFirst xs' (pred fileLength) of
                Nothing -> compactify2 (largestValue - 1) (take (fromInteger idx) xs') ++ drop (fromInteger idx) xs'
                Just (idxEmpty, Left k) ->
                    if idx > idxEmpty then
                        compactify2 (largestValue - 1) (take (fromInteger idxEmpty) xs'
                        ++ [Right (fileLength,fileIdx), Left (k-fileLength)]
                        ++ (take (fromInteger (idx-idxEmpty-1)) (drop (fromInteger idxEmpty + 1) xs')))
                        ++ [Left fileLength]
                        ++ (drop (fromInteger (idx+1)) xs')
                    else
                        compactify2 (largestValue - 1) (take (fromInteger idx) xs') ++ drop (fromInteger idx) xs'
    where
        xs' = compactify2' ys
        pred l x = case x of
            Left k -> k >= l
            _ -> False
        predIsRight val x = case x of
            Right (fileLength, fileIdx) -> fileIdx == val
            _ -> False
            

compactify2' :: [Either Integer (Integer, Integer)] -> [Either Integer (Integer, Integer)]
compactify2' ((Left a):(Left b):xs) = compactify2' $ (Left $ a+b):xs
compactify2' ((Left 0):(Right b):xs) = compactify2' $ (Right b):xs
compactify2' (x:xs) = x:(compactify2' xs)
compactify2' [] = []


getChecksum :: Integer -> Integer -> [Integer] -> Integer
getChecksum idx acc [] = acc
getChecksum idx acc (x:xs) = getChecksum (idx + 1) (acc + idx*x) xs

solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let parsedInput = parseInput ((lines file)!!0)
    let expandedInput = expandInput 0 parsedInput
    let compactifiedInput = compactify expandedInput
    case sequence compactifiedInput of
        Just xs -> return $ getChecksum 0 0 xs
        Nothing -> return 0

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let parsedInput = parseInput ((lines file)!!0)
    let expandedInput2 = expandInput2 0 parsedInput
    let compactifiedInput2 = compactify2 20000 expandedInput2
    return $ getChecksum 0 0 $ expandInput2Again $  compactify2' compactifiedInput2



main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

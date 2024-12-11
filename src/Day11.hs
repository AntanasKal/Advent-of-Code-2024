
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
import Data.Map

simulateMemo :: [Integer] -> Integer -> State (Map (Integer, Integer) Integer) Integer
simulateMemo [] iters = pure 0
simulateMemo xs 0 = pure $ fromIntegral $ length xs
simulateMemo (x:xs) iters = do
    currMap <- get
    case Data.Map.lookup (x, iters) currMap of
        Just a -> do
            theRest <- simulateMemo xs iters
            pure $ a + theRest
        Nothing -> do
            let x' = simulate1 x
            resultFromX <- simulateMemo x' (iters-1)
            currMap <- get
            put (Data.Map.insert (x, iters) resultFromX currMap)
            theRest <- simulateMemo xs iters
            pure $ resultFromX + theRest


simulate1 :: Integer -> [Integer]
simulate1 0 = [1]
simulate1 x = if l `mod` 2 == 0 then
    [read $ Data.List.take (l `div` 2) (show x), read $ Data.List.drop (l `div` 2) (show x)]
    else
        [x*2024]
    where
        l = length $ show x


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let numbers = Data.List.map (read) $ splitOn " " (lines file!!0) :: [Integer]
    let (theRes, theMap) = runState (simulateMemo numbers 25) empty
    pure theRes

solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let numbers = Data.List.map (read) $ splitOn " " (lines file!!0) :: [Integer]
    let (theRes, theMap) = runState (simulateMemo numbers 75) empty
    pure theRes


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

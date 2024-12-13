{-# LANGUAGE RankNTypes, TemplateHaskell #-}
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
import Control.Monad.Identity
import Control.Monad.Union
import Data.Union
import Control.Monad
import Data.Set

data MapElement = Element Char
    deriving (Ord, Eq)

test :: UnionM (Integer, Integer) [Node]
test = do
    n1 <- new (1, 2)
    n2 <- new (2, 3)
    n3 <- new (3, 4)
    merge  (\x y -> (y, y)) n1 n2
    merge  (\x y -> (y, y)) n1 n3
    return [n1,n2,n3]

test2 :: UnionM (Integer, Integer) (Integer, Integer)
test2 = do
    arr <- test
    (_, l1) <- Control.Monad.Union.lookup (arr!!0)
    return l1


constructInitialUnions :: Array (Integer, Integer) MapElement -> UnionM (Integer, Integer) (Array (Integer, Integer) (Node, MapElement))
constructInitialUnions arr = do
    list <- mapM action [(i,j) | i <- [0..h], j<-[0..w]]
    pure $ array ((0,0), (h,w)) [((i,j), n) | (i,j,n) <- list]
    where
        (_, (h, w)) = bounds arr
        action :: (Integer, Integer) -> UnionM (Integer, Integer) (Integer, Integer, (Node, MapElement))
        action (i,j) = do
            n1 <- new (i,j)
            let mapElement = arr!(i,j)
            pure (i,j,(n1, mapElement))
        
unioniseNeighbours :: (Array (Integer, Integer) (Node, MapElement)) -> UnionM (Integer, Integer) ()
unioniseNeighbours arr =
    mapM_ action [(i,j) | i <- [0..h], j<-[0..w]]
    where
        (_, (h, w)) = bounds arr
        action :: (Integer, Integer) -> UnionM (Integer, Integer) ()
        action (i,j) = do
            when
                (i > 0 && snd (arr ! (i - 1, j)) == snd (arr ! (i, j)))
                (merge2 (i-1,j) (i,j))
            when
                (j > 0 && snd (arr ! (i, j-1)) == snd (arr ! (i, j)))
                (merge2 (i,j-1) (i,j))
            when
                (i < h && snd (arr ! (i + 1, j)) == snd (arr ! (i, j)))
                (merge2 (i+1,j) (i,j))
            when
                (j < w && snd (arr ! (i, j+1)) == snd (arr ! (i, j)))
                (merge2 (i,j+1) (i,j))
        
        pickFirst x y = (x,x)
        merge2 (i1,j1) (i2,j2) = void
            (merge pickFirst (fst (arr ! (i1, j1))) (fst $ arr ! (i2, j2)))


getTheFinalUnion :: Array (Integer, Integer) MapElement -> (Union (Integer, Integer), Array (Integer, Integer) (Node, MapElement))
getTheFinalUnion arr = run' (
    do
        stuff <- constructInitialUnions arr
        unioniseNeighbours stuff
        flatten
        pure stuff
    )
    

getDistinctElemsHelper ::  Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Set Node -> [(Integer, Integer)] -> Set Node
getDistinctElemsHelper unn arrMap setSoFar [] = setSoFar
getDistinctElemsHelper unn arrMap setSoFar (x:xs) =
    getDistinctElemsHelper unn arrMap (Data.Set.insert representativeNode setSoFar) xs
    where
        (representativeNode, _) = Data.Union.lookup unn nodeOfX
        (nodeOfX, _) = arrMap!x


getDistinctElems :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> [Node]
getDistinctElems  unn arrMap = Data.Set.elems $ getDistinctElemsHelper unn arrMap Data.Set.empty [(i,j)| i<-[0..h], j<-[0..w]] 
    where
        (_, (h, w)) = bounds arrMap


parseInput :: String -> Array (Integer, Integer) MapElement
parseInput input = mapArray
    where
        list = Data.List.map (Data.List.map Element) (lines input)
        h = fromIntegral $ length list - 1
        w = fromIntegral $ (length $ head list) - 1
        mapArray = array ((0,0), (h,w)) [((i,j), list!! fromIntegral i !! fromIntegral j) | i <- [0..h], j <- [0..w]]


validDifferentNodes :: Union (Integer, Integer) -> Array (Integer, Integer) (Node, MapElement) -> (Integer, Integer) -> (Integer, Integer) -> Bool
validDifferentNodes unn arrMap (i1,j1) (i2,j2) =
    if   i1 >= 0
    && i1 <= h
    && i2 >= 0
    && i2 <= h
    && j1 >= 0
    && j1 <= w
    && j2 >= 0
    && j2 <= h
    then fst (Data.Union.lookup unn (fst $ arrMap!(i1,j1))) /= fst (Data.Union.lookup unn (fst $ arrMap!(i2,j2)))
    else
        True
    where
        (_, (h, w)) = bounds arrMap

getPerimeter :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Node -> Integer
getPerimeter unn arrMap node = sum $ [ countExternalWalls (i,j) | i <- [0..h], j <- [0..w], fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node]
    where
        (_, (h, w)) = bounds arrMap
        countExternalWalls (i, j) = fromIntegral $ length $ Data.List.filter (validDifferentNodes unn arrMap (i,j)) [(i-1,j), (i+1,j), (i,j-1), (i,j+1)]

getArea :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Node -> Integer
getArea unn arrMap node = fromIntegral $ length $ [() | i <- [0..h], j <- [0..w], fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node]
    where
        (_, (h, w)) = bounds arrMap

getSidesVertical1Helper :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Integer -> Node -> [Bool]
getSidesVertical1Helper unn arrMap j node =
    if j ==0 then 
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node |i<-[0..h]]
    else
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node
          && fst (Data.Union.lookup unn (fst (arrMap!(i,j-1)))) /= node  
         |i<-[0..h]]
    where
        (_, (h, w)) = bounds arrMap

getSidesVertical2Helper :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Integer -> Node -> [Bool]
getSidesVertical2Helper unn arrMap j node =
    if j == w then 
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node |i<-[0..h]]
    else
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node
          && fst (Data.Union.lookup unn (fst (arrMap!(i,j+1)))) /= node  
         |i<-[0..w]]
    where
        (_, (h, w)) = bounds arrMap

getSidesHorizontal1Helper :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Integer -> Node -> [Bool]
getSidesHorizontal1Helper unn arrMap i node =
    if i ==0 then 
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node |j<-[0..w]]
    else
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node
          && fst (Data.Union.lookup unn (fst (arrMap!(i-1,j)))) /= node  
         |j<-[0..w]]
    where
        (_, (h, w)) = bounds arrMap

getSidesHorizontal2Helper :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Integer -> Node -> [Bool]
getSidesHorizontal2Helper unn arrMap i node =
    if i == h then 
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node |j<-[0..w]]
    else
        [fst (Data.Union.lookup unn (fst (arrMap!(i,j)))) == node
          && fst (Data.Union.lookup unn (fst (arrMap!(i+1,j)))) /= node  
         |j<-[0..h]]
    where
        (_, (h, w)) = bounds arrMap

countRisingEdges :: [Bool] -> Integer
countRisingEdges (False:True:xs) = 1 + countRisingEdges(True:xs)
countRisingEdges (True:xs) = countRisingEdges xs
countRisingEdges (False:False:xs) = countRisingEdges (False:xs)
countRisingEdges [] = 0
countRisingEdges [False] = 0

getHorizontalSides :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Node -> Integer
getHorizontalSides unn arrMap node =
    sum $ [countRisingEdges (False:getSidesHorizontal1Helper unn arrMap i node) + countRisingEdges (False:getSidesHorizontal2Helper unn arrMap i node)  | i <- [0..h]]
    where
        (_, (h, w)) = bounds arrMap

getVerticalSides :: Union (Integer, Integer) ->  Array (Integer, Integer) (Node, MapElement) -> Node -> Integer
getVerticalSides unn arrMap node =
    sum $ [countRisingEdges (False:getSidesVertical1Helper unn arrMap j node) + countRisingEdges (False:getSidesVertical2Helper unn arrMap j node)  | j <- [0..w]]
    where
        (_, (h, w)) = bounds arrMap

getSides unn arrMap node = getHorizontalSides unn arrMap node + getVerticalSides unn arrMap node


solvePart1 :: String -> IO Integer
solvePart1 path = do
    file <- readFile path
    let array = parseInput file
    let (unn, nodes) = getTheFinalUnion array
    let distinctElems = getDistinctElems unn nodes
    print $ length distinctElems
    return $ sum $ Data.List.map (\n -> (getPerimeter unn nodes n)*(getArea unn nodes n)) distinctElems


solvePart2 :: String -> IO Integer
solvePart2 path = do
    file <- readFile path
    let array = parseInput file
    let (unn, nodes) = getTheFinalUnion array
    let distinctElems = getDistinctElems unn nodes
    print $ length distinctElems

    return $ sum $ Data.List.map (\n -> (getSides unn nodes n)*(getArea unn nodes n)) distinctElems


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["part1", path] -> solvePart1 path >>= print
        ["part2", path] -> solvePart2 path >>= print
        _ -> error "Wrong argument format."

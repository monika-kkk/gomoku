module Board
( Board(..)
, Symbol(..)
, Pos(..)
, dim
, getSymbol
, putSymbol
, showBoard
, initBoard
, concatListOfSymbols
, getOppositeSymbol
, sequencesThatContainsPosition
, statesOnBoard
, positionsOnBoard
) where

import           Data.List
import           Data.Matrix

import qualified Data.Vector as Vector

type Pos = (Int, Int)
data Symbol = X | O | Empty deriving (Eq, Read)
type Board = Matrix Symbol

dim :: Int
dim = 19

instance Show Symbol where
    show X     = "X"
    show O     = "O"
    show Empty = "_"

numberAlignedRight :: Int -> String
numberAlignedRight n
    | n < 10 = " " ++ show n ++ " "
    | otherwise = show n ++ " "

rowWithColsNumbers :: String
rowWithColsNumbers = "   "
                    ++ concatMap numberAlignedRight [1..dim] ++ "\n"

rowToString :: Int -> Board -> String
rowToString n board = numberAlignedRight n
                   ++ " "
                   ++ concatMap (\symbol -> show symbol ++ "  ") symbolsInRow
                   ++ numberAlignedRight n
                   where symbolsInRow = getRow n board

showBoard :: Board -> String
showBoard board = rowWithColsNumbers
                ++ unlines [rowToString row board | row <- [1..dim]]
                ++ "\n" ++ rowWithColsNumbers

initBoard :: Board
initBoard = matrix dim dim (\(i,j) -> Empty)

putSymbol :: Symbol -> Pos -> Board -> Board
putSymbol = setElem

getSymbol :: Pos -> Board -> Symbol
getSymbol (x,y) = getElem x y

getOppositeSymbol symbol
    | symbol == X = O
    | symbol == O = X

positionsOnBoard :: [Pos]
positionsOnBoard = [(x,y) | x <- [1..dim], y <- [1..dim]]

concatListOfSymbols :: [Symbol] -> String
concatListOfSymbols = concatMap show

statesInRow :: Pos -> Board -> String
statesInRow (row,_) board = concatListOfSymbols (Vector.toList $ getRow row board)

statesInCol :: Pos -> Board -> String
statesInCol (_, col) board = concatListOfSymbols (Vector.toList $ getCol col board)

statesDiagonalFromLeft :: Pos -> Board -> String
statesDiagonalFromLeft (row, col) board =
    concatListOfSymbols [getElem x y board | (x,y) <- positionsOnBoard, (row - col) == (x - y)]

statesDiagonalFromRight :: Pos -> Board -> String
statesDiagonalFromRight (row, col) board =
    concatListOfSymbols [getElem x y board | (x,y) <- positionsOnBoard, (row - x) == (y - col)]

sequencesThatContainsPosition :: Pos -> Board -> [String]
sequencesThatContainsPosition pos board = [f pos board | f <- functions]
    where functions = [statesInRow, statesInCol, statesDiagonalFromLeft, statesDiagonalFromRight]

statesOnBoard :: Board -> [String]
statesOnBoard board = statesDiagonalLeft ++ statesDiagonalRight ++ [f (x, x) board | f <- functions, x <- [1..dim]]
                    where functions = [statesInRow, statesInCol]
                          statesDiagonalLeft = [statesDiagonalFromLeft (x, dim - x + 1) board | x <- [1..dim]]
                                            ++ [statesDiagonalFromLeft (x + 1, dim - x + 1) board | x <- [1..dim-2]]
                          statesDiagonalRight = [statesDiagonalFromRight (x, x) board | x <- [1..dim]]
                                            ++ [statesDiagonalFromRight (x, x + 1) board | x <- [1..dim-1]]


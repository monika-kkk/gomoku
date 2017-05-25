module Board
( initBoard
, putSymbol
, statesInRow
, statesInCol
, gameIsOver
, Symbol(..)
, Board(..)
, Pos(..)
, showBoard
) where

import Data.Matrix
-- import Data.Vector
import Data.Char
import Data.List
import Data.String
import Data.String.Utils
import Prelude
import Data.List.Split
import Debug.Trace

-- {-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
-- :set -XTypeSynonymInstances

type Pos = (Int, Int)
data Symbol = Empty | X | O deriving (Eq, Read)
type Board = Matrix Symbol
-- newtype Board = Board {getBoard :: Matrix Symbol}
--type/newtype/data <- diff, problem with newtype, show with type, const def

instance Show Symbol where
    show X = "X"
    show O = "O"
    show Empty = "_"

-- instance Show Board where
--     show = boardToString

dim :: Int
dim = 11

initBoard :: Board
initBoard = matrix dim dim (\(i,j) -> Empty)

-- concatVectorOfSymbols :: Vector Symbol -> String
-- first usage => type ?
concatVectorOfSymbols = foldr (\symbol result -> show symbol ++ result) ""
concatListOfSymbols = foldr (\symbol result -> show symbol ++ result) ""

rowToString :: Int -> Board -> String
rowToString n board = formattedNumber n ++ "  " ++ foldr (\symbol result -> show symbol ++ "  " ++ result) "" (getRow n board)

formattedNumber :: Int -> String
formattedNumber n
    | n < 10 = " " ++ show n
    | otherwise = show n

colsNumbersString :: String
colsNumbersString = "   " ++ foldr (\n str -> formattedNumber n ++ " " ++ str) "" [1..dim] ++ "\n"

boardToString :: Board -> String
boardToString board = colsNumbersString ++ unlines [rowToString row board | row <- [1..dim]]

showBoard = boardToString

putSymbol :: Symbol -> Pos -> Board -> Board
putSymbol = setElem

-------------------------------------------------------------

statesInRow :: Int -> Board -> String
statesInRow n board = concatVectorOfSymbols $ getRow n board

statesInCol :: Int -> Board -> String
statesInCol n board = concatVectorOfSymbols $ getCol n board

statesDiagonalFromLeft :: Pos -> Board -> String
statesDiagonalFromLeft (row,col) board =
    concatListOfSymbols [getElem x y board |  x <- [1..dim], y <- [1..dim], (row-x) == (col-y)]

statesDiagonalFromRight :: Pos -> Board -> String
statesDiagonalFromRight (row,col) board =
    concatListOfSymbols [getElem x y board | x <- [1..dim], y <- [1..dim], (row-x) == (y-col)]

gameIsOver :: Pos -> Board -> Bool
gameIsOver (row, col) board = any (winningSetting symbol board) allLists
    where rowAsString = statesInRow row board
          colAsString = statesInCol col board
          diagonalFromLeftAsString = statesDiagonalFromLeft (row,col) board
          diagonalFromRightAsString = statesDiagonalFromRight (row,col) board
          allLists = [rowAsString,colAsString,diagonalFromLeftAsString,diagonalFromRightAsString]
          symbol = getElem row col board

winningSetting :: Symbol -> Board -> String -> Bool
-- winningSetting sym board valAsString | Debug.Trace.trace ("Params: " ++ show sym ++ " " ++ valAsString) False = undefined
winningSetting sym board valAsString
    | sym == O = any (\val -> isPrefixOf val valAsString) prefixesO ||
                 any (\val -> isSuffixOf val valAsString) suffixesO ||
                 any (\val -> isInfixOf val valAsString) infixesO
    | sym == X = any (\val -> isPrefixOf val valAsString) prefixesX ||
                 any (\val -> isSuffixOf val valAsString) suffixesX ||
                 any (\val -> isInfixOf val valAsString) infixesX
    where prefixesO = ["OOOOOX","OOOOO_"]
          suffixesO = ["XOOOOO","_OOOOO"]
          infixesO = ["XOOOOOX","_OOOOO_","XOOOOO_","_OOOOOX"]
          prefixesX = ["XXXXXO","XXXXX_"]
          suffixesX = ["OXXXXX","_XXXXX"]
          infixesX = ["OXXXXXO","_XXXXX_","OXXXXX_","_XXXXXO"]
--empty, < 5

-----------------------------------------------------------------


-- paramethers order, files, tests, enum,  nested functions
-- Symbol/Empty
-- import/export cleanup

module Board where

import Data.Matrix
-- import Data.Vector
import Data.Char
import Data.List
import Data.String
import Data.String.Utils
import Prelude
import Data.List.Split

-- {-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
-- :set -XTypeSynonymInstances

type Pos = (Int, Int)
data Symbol = Empty | X | O deriving Eq
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
dim = 3

initBoard :: Board
initBoard = matrix dim dim (\(i,j) -> Empty)

-- concatVectorOfSymbols :: Vector Symbol -> String
concatVectorOfSymbols = foldr (\symbol result -> show symbol ++ result) ""

rowToString :: Int -> Board -> String
rowToString n board = concatVectorOfSymbols (getRow n board)

-- todo: num
boardToString :: Board -> String
boardToString board = unlines [rowToString row board | row <- [1..dim]]

putSymbol :: Symbol -> Pos -> Board -> Board
putSymbol = setElem

-------------------------------------------------------------

statesInRow :: Int -> Board -> String
statesInRow n board = concatVectorOfSymbols $ getRow n board

statesInCol :: Int -> Board -> String
statesInCol n board = concatVectorOfSymbols $ getCol n board

gameIsOver :: Pos -> Board -> Bool
gameIsOver (row, col) board = any (winningSetting symbol board) allLists
    where rowAsString = statesInRow row board
          colAsString = statesInCol col board
          crossLeftAsString = []
          crossRightAsString = []
          allLists = [rowAsString,colAsString,crossLeftAsString,crossRightAsString]
          symbol = getElem row col board

winningSetting :: Symbol -> Board -> String -> Bool
winningSetting sym board valAsString
    | sym == O = any (isPrefixOf valAsString) prefixesO ||
                 any (isSuffixOf valAsString) suffixesO ||
                 any (isInfixOf valAsString) infixesO
    | sym == X = any (isPrefixOf valAsString) prefixesX ||
                 any (isSuffixOf valAsString) suffixesX ||
                 any (isInfixOf valAsString) infixesX
    where prefixesO = ["OOOOOX","OOOOO_"]
          suffixesO = ["XOOOOO","_OOOOO"]
          infixesO = ["XOOOOOX","_OOOOO_","XOOOOO_","_OOOOOX"]
          prefixesX = ["XXXXXO","XXXXX_"]
          suffixesX = ["OXXXXX","_XXXXX"]
          infixesX = ["OXXXXXO","_XXXXX_","OXXXXX_","_XXXXXO"]




-- paramethers order, files, tests, enum,  nested functions

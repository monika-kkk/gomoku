module Board
( initBoard
, putSymbol
, isWinningSetting
, gameIsOver
, Symbol(..)
, Board(..)
, Pos(..)
, showBoard
) where

import Data.List
import Data.Matrix
import qualified Data.Vector as V

type Pos = (Int, Int)
data Symbol = X | O | Empty deriving (Eq, Read)
type Board = Matrix Symbol

instance Show Symbol where
    show X = "X"
    show O = "O"
    show Empty = "_"

formattedNumber :: Int -> String
formattedNumber n
    | n < 10 = " " ++ show n
    | otherwise = show n

rowWithColsNumbers :: String
rowWithColsNumbers = "   "
                  ++ foldr (\n str -> formattedNumber n ++ " " ++ str) "" [1..dim]
                  ++ "\n"

rowToString :: Int -> Board -> String
rowToString n board = formattedNumber n
                   ++ "  "
                   ++ foldr (\symbol result -> show symbol ++ "  " ++ result) "" (getRow n board)

showBoard :: Board -> String
showBoard board = rowWithColsNumbers ++ unlines [rowToString row board | row <- [1..dim]]

concatListOfSymbols :: [Symbol] -> String
concatListOfSymbols = concatMap show

-------------------------------------------------------------

dim :: Int
dim = 11

initBoard :: Board
initBoard = matrix dim dim (\(i,j) -> Empty)

putSymbol :: Symbol -> Pos -> Board -> Board
putSymbol = setElem

oppositeSymbol symbol
    | symbol == X = O
    | symbol == O = X

statesInRow :: Int -> Board -> String
statesInRow n board = concatMap show (V.toList $ getRow n board)

statesInCol :: Int -> Board -> String
statesInCol n board = concatMap show (V.toList $ getCol n board)

statesDiagonalFromLeft :: Pos -> Board -> String
statesDiagonalFromLeft (row,col) board =
    concatListOfSymbols [getElem x y board |  x <- [1..dim], y <- [1..dim], (row-x) == (col-y)]

statesDiagonalFromRight :: Pos -> Board -> String
statesDiagonalFromRight (row,col) board =
    concatListOfSymbols [getElem x y board | x <- [1..dim], y <- [1..dim], (row-x) == (y-col)]

--------------------------------------------------

data Type = I | P | S

generateStates :: Symbol -> Type -> [String]
generateStates symbol t = case t of
                            I -> [pref ++ replFive ++ suff | pref <- otherSymbols, suff <- otherSymbols]
                            P -> [replFive ++ suff | suff <- otherSymbols]
                            S -> [pref ++ replFive | pref <- otherSymbols]
                            where otherSymbols = ["_", show $ oppositeSymbol symbol]
                                  replFive = concatListOfSymbols $ replicate 5 symbol

isWinningSetting :: Symbol -> String -> Bool
isWinningSetting lastInsertedSymbol statesAsString =
                 any (`isPrefixOf` statesAsString) (generateStates lastInsertedSymbol P) ||
                 any (`isSuffixOf` statesAsString) (generateStates lastInsertedSymbol S) ||
                 any (`isInfixOf` statesAsString) (generateStates lastInsertedSymbol I)

gameIsOver :: Pos -> Board -> Bool
gameIsOver lastInsertedPos@(r, c) board = any (isWinningSetting symbol) statesToCheck
    where row = statesInRow r board
          col = statesInCol c board
          diagLeft = statesDiagonalFromLeft (r,c) board
          diagRight = statesDiagonalFromRight (r,c) board
          statesToCheck = [row, col, diagLeft, diagRight]
          symbol = getElem r c board

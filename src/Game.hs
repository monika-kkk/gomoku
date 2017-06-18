module Game
( Game(..)
, initGame
, isGameOver
) where

import           Board
import           Data.List
import           Data.Matrix

newtype Game = Game { board :: Board}

initGame :: Game
initGame = Game initBoard

isGameOver :: Pos -> Board -> Bool
isGameOver pos@(row,col) board = any (containsWinningSetting symbol) statesToCheck
       where statesToCheck = sequencesThatContainsPosition pos board
             symbol = getElem row col board

data SeqType = I | P | S

containsWinningSetting :: Symbol -> String -> Bool
containsWinningSetting symbol statesAsString =
                 any (`isPrefixOf` statesAsString) (generateStates symbol P) ||
                 any (`isSuffixOf` statesAsString) (generateStates symbol S) ||
                 any (`isInfixOf` statesAsString) (generateStates symbol I)

generateStates :: Symbol -> SeqType -> [String]
generateStates symbol t = case t of
                            I -> [pref ++ fiveSymbols ++ suff | pref <- otherSymbols, suff <- otherSymbols]
                            P -> [fiveSymbols ++ suff | suff <- otherSymbols]
                            S -> [pref ++ fiveSymbols | pref <- otherSymbols]
                            where otherSymbols = ["_", show $ getOppositeSymbol symbol]
                                  fiveSymbols = concatListOfSymbols $ replicate 5 symbol


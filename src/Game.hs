module Game
  ( Game(..)
  , initGame
  ) where

import Board

data Game = Game
  { board :: Board
  , aiSymbol :: Symbol
  , userSymbol :: Symbol
  , firstTurn :: Symbol
  } deriving (Show)

initGame :: Symbol -> Symbol -> Game
initGame userSymbol = Game initBoard (getOppositeSymbol userSymbol) userSymbol

getOppositeSymbol :: Symbol -> Symbol
getOppositeSymbol X = O
getOppositeSymbol O = X

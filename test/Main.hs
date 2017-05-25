module Main
  ( main
  ) where

import Board
import Test.HUnit
import Test.Hspec

-- import Control.Exception (evaluate)
main :: IO ()
main =
  hspec $ do
    describe "gameIsOver" $ do
      it "returns false if there is no five symbols in the row, col or on the diagonal" $ do
        (gameIsOver (2, 4) $ putSymbol X (2, 4) initBoard) @?= False


-- quickCheck - random values, general approach

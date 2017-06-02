module Main
  ( main
  ) where

import Board
import Test.HUnit
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main =
  hspec $ do
    describe "Board" $ do
      describe "isWinningSetting" $ do
        it "returns false if there is no five symbols side by side" $ do
            isWinningSetting X "XXXX_OO" @?= False
            isWinningSetting O "_OOOOOO" @?= False
            isWinningSetting X "OXXXXOOXOOOOX" @?= False
        it "returns true if there is exactly five symbols side by side" $ do
            isWinningSetting X "XXXXX_XXX" @?= True
            isWinningSetting O "_XXOOOOO_" @?= True
            isWinningSetting O "_XXOOOOO" @?= True
        it "returns false if there is too many symbols" $ do
            isWinningSetting X "XXXXXXOO" @?= False
            isWinningSetting O "_OOOOOOXX" @?= False

module Main where

import Data.Matrix
import Control.Monad
import Board
import Game

main :: IO ()
main = do
    putStrLn "Choose symbol O | X"
    line <- getLine
    let userSymbol :: Symbol
        userSymbol = read line

    putStrLn "Choose whose turn first"
    line <- getLine
    let firstTurn :: Symbol
        firstTurn = read line

    let game = initGame userSymbol firstTurn
    gameLoop game
    putStrLn "THE END"

gameLoop :: Game -> IO ()
gameLoop game = do
    let oldBoard = board game
    putStrLn $ showBoard oldBoard

    putStrLn "Your turn. Write coords separated with newline character"
    line <- getLine
    let x :: Int
        x = read line
    line <- getLine
    let y :: Int
        y = read line
    let coords = (x,y)

    let newBoard = putSymbol (userSymbol game) coords oldBoard

    unless (gameIsOver coords newBoard) (gameLoop game { board = newBoard})
    putStrLn ""

-- wrong data, nonempty field
-- read move, update board, check state, AI move, update board, check state ...
-- show result
-- exactly 5 stones (?)
-- contents
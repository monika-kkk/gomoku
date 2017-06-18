module Main where

import           Board
import           MinMax
import           Game
import           Data.Matrix
import           Data.Maybe
import           Text.Read

main :: IO ()
main = do
    let game = initGame
    userTurn game
    return()

userTurn :: Game -> IO ()
userTurn game = do
    let prevBoard = board game
    putStrLn $ showBoard prevBoard
    putStrLn "Your turn. Write coords separated with newline character"
    pos <- readAndCheckPosition prevBoard
    let newBoard = putSymbol O pos prevBoard
    if isGameOver pos newBoard then putStrLn (showBoard newBoard ++ "User won") else aiTurn game { board = newBoard}
    return ()

aiTurn :: Game -> IO ()
aiTurn game = do
    let prevBoard = board game
    let (pos, newBoard) = aiMove prevBoard
    if isGameOver pos newBoard then putStrLn (showBoard newBoard ++ "AI won") else userTurn game { board = newBoard}
    return ()

readAndCheckPosition :: Board -> IO (Int, Int)
readAndCheckPosition board = do
    (x,y) <- readAndParseInt
    if x < 1 || x > 19 || y < 1 || y > 19 || getSymbol (x,y) board /= Empty
        then putStrLn "Coords outside the board or nonempty field" >> readAndCheckPosition board
    else return (x,y)

readAndParseInt :: IO (Int, Int)
readAndParseInt = do
    x <- getLine
    y <- getLine
    case (readMaybe x, readMaybe y) of
        (Just a, Just b) -> return (a,b)
        _                -> putStrLn "Wrong format. Write coords separated with newline character" >> readAndParseInt




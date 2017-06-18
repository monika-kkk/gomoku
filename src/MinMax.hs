module MinMax
( isGameOver
, aiMove
) where

import           Board
import           Game
import           Data.Tree
import           Data.List

aiMove :: Board -> BoardWithLastPos
aiMove board = rootLabel (children !! maxIndex)
    where (Node _ children) = buildTree O 3 ((-1,-1), board)
          minimaxList  = map (\node -> findMin node infinity infinityMinus infinity) children
          (Just maxIndex) = elemIndex (maximum minimaxList) minimaxList

findMax :: Tree BoardWithLastPos -> Int -> Int -> Int -> Int
findMax (Node boardWithPos []) _ _ _ = rateBoard boardWithPos
findMax (Node boardWithPos (currentChildBoard:childrenBoards)) oldValue oldAlpha beta
    | beta <= alpha = value
    | null childrenBoards = value
    | otherwise = findMax (Node boardWithPos childrenBoards) value alpha beta
    where value = max oldValue (findMin currentChildBoard infinity oldAlpha beta)
          alpha = max oldAlpha value

findMin :: Tree BoardWithLastPos -> Int -> Int -> Int -> Int
findMin (Node boardWithPos []) _ _ _ = rateBoard boardWithPos
findMin (Node boardWithPos (currentChildBoard:childrenBoards)) oldValue alpha oldBeta
    | beta <= alpha = value
    | null childrenBoards = value
    | otherwise = findMin (Node boardWithPos childrenBoards) value alpha beta
    where value = min oldValue (findMax currentChildBoard infinityMinus alpha oldBeta)
          beta = min oldBeta value

rateBoard :: BoardWithLastPos -> Int
rateBoard (pos,board)
    | isGameOver pos board && symbol == O = -10000
    | isGameOver pos board && symbol == X = 10000
    | otherwise = sum $ map rateSequence statesToCheck
       where statesToCheck = statesOnBoard board
             symbol = getSymbol pos board

rateSequence ::  String -> Int
rateSequence  string =  fourX + threeX - fourO - threeO + counterX - counterO
    where fourX = 1000 * infixCounter ["X_XXX","XX_XX","XXX_X","_XXXX","XXXX_"]
          fourO = 1000 * infixCounter ["O_OOO","OO_OO","OOO_O","_OOOO","OOOO_"]
          threeX = 10 * infixCounter ["XXX__","XX__X","X__XX","__XXX","_X_XX","X_X_X","XX_X_"]
          threeO = 10 * infixCounter ["OOO__","OO__O","O__OO","__OOO","_O_OO","O_O_O","OO_O_"]
          counterX = length $ filter (== 'X') string
          counterO = length $ filter (== 'O') string
          infixCounter infixes = length $ filter (`isInfixOf` string) infixes

type BoardWithLastPos = (Pos, Board)

buildTree:: Symbol -> Int -> BoardWithLastPos -> Tree BoardWithLastPos
buildTree lastInsertedSymbol level boardWithLastPos@(pos, board)
    | level == 0 = Node boardWithLastPos []
    | pos /= (-1,-1) && isGameOver pos board = Node boardWithLastPos []
    | otherwise = Node boardWithLastPos (map (buildTree oppositeSymbol (level - 1)) possibleBoards)
    where oppositeSymbol = getOppositeSymbol lastInsertedSymbol
          possibleBoards = generatePossibleBoards board oppositeSymbol

generatePossibleBoards :: Board -> Symbol -> [BoardWithLastPos]
generatePossibleBoards board symbol = map (\pos -> (pos, putSymbol symbol pos board)) availablePositions
    where availablePositions = getEmptyFieldsWithNeighbours board

getEmptyFieldsWithNeighbours :: Board -> [Pos]
getEmptyFieldsWithNeighbours board = [pos | pos <- positionsOnBoard, let state = getSymbol pos board, state == Empty,
                                                                                                      hasNeighbour pos board]

hasNeighbour :: Pos -> Board -> Bool
hasNeighbour pos board = any (\p -> getSymbol p board /= Empty) (generateNeighboursPositions pos)

generateNeighboursPositions :: Pos -> [Pos]
generateNeighboursPositions (x,y) = [(a,b) | (a,b) <- neighbours, a >=1, b >= 1, a <= dim, b <= dim]
    where neighbours = [(x,y+1),(x,y-1),(x+1,y),(x+1,y-1),(x+1,y+1),(x-1,y-1),(x-1,y),(x-1,y+1)]

infinity = 1000000
infinityMinus = -infinity

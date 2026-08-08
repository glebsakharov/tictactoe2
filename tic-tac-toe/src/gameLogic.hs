
{-# LANGUAGE TemplateHaskell #-}

module GameLogic (Player (..),
				  Board,
				  GameState (..),
				  GameAction (..),
				  GameResult (..),
				  initialState,
				  isValidMove,
				  placeMark,
				  hasWon,
				  isBoardFull,
				  processMove,
				  switchPlayer,
				  checkWinner,
				  getGameResultMessage) where 

import Data.List hiding (ix)
import Control.Lens 
import Data.Maybe (isJust, fromJust)

data Player = X | O deriving (Show, Eq)

type Board = [[Maybe Player]]

data GameState = GameState
  { _board :: Board
  , _currentPlayer :: Player
  , _winner :: Maybe Player
  , _gameOver :: Bool
  , _undoStack :: [GameState] -- past states
  , _redoStack :: [GameState] -- future states
  } deriving (Show, Eq)

makeLenses ''GameState 

initialState :: GameState
initialState = GameState emptyBoard X Nothing False [][]

emptyBoard :: Board 
emptyBoard = replicate 3 (replicate 3 Nothing)


--pure game logic 
isValidMove :: Board -> (Int, Int) -> Bool 
isValidMove board (row,col) = (not $ isJust (board !! row !! col)) 
                && elem (row,col) [(x,y) | x <- [0,1,2], y <- [0,1,2]]


placeMark :: Board -> (Int, Int) -> Player -> Board         
placeMark board (row,col) player 
  | isValidMove board (row,col) = board & ix row . ix col .~ Just player
  | otherwise = board 


hasWon :: Board -> Player -> Bool
hasWon board player = any winCondition linesToCheck
  where
    linesToCheck = board <> transpose board <> diagonals
    diagonals = [[board !! i !! i | i <- [0..2]],
                 [board !! i !! (2 - i) | i <- [0..2]]]
    winCondition line = all (== Just player) line


isBoardFull :: Board -> Bool 
isBoardFull board = all (/= Nothing) (concat board)


data GameAction = Move (Int, Int) | Quit
data GameResult = 
    StateUpdate GameState      
  | InvalidMove String         
  | GameAlreadyOver String     


processMove :: GameState -> (Int, Int) -> GameResult
processMove state (row, col)
  | state ^. gameOver = GameAlreadyOver "Game is already finished"
  | not (isValidMove (state ^. board) (row, col)) = 
      InvalidMove "Cell occupied or out of bounds"
  | otherwise = 
      let newBoard = placeMark (state ^. board) (row, col) (state ^. currentPlayer)
          playerWon = hasWon newBoard (state ^. currentPlayer)
          boardFull = isBoardFull newBoard
          gameOver' = playerWon || boardFull
          winner' = if playerWon then Just (state ^. currentPlayer) else Nothing
          nextPlayer = if not gameOver' then switchPlayer (state ^. currentPlayer) 
                       else state ^. currentPlayer
          newUndoStack = state : (state ^. undoStack)
          newRedoStack = []
      in StateUpdate $ state & board .~ newBoard
                         & currentPlayer .~ nextPlayer
                         & winner .~ winner'
                         & gameOver .~ gameOver'
                         & undoStack .~ newUndoStack
                         & redoStack .~ newRedoStack

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X 

checkWinner :: GameState -> Maybe Player
checkWinner state = state ^. winner 


getGameResultMessage :: GameState -> String
getGameResultMessage state = case state ^. winner of
  Just p -> "Player " ++ show p ++ " wins!"
  Nothing -> if isBoardFull (state ^. board) then "It's a tie!" else ""








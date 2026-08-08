{-# LANGUAGE TemplateHaskell #-}

module Runner (TicTacToe,
			   handlePlayerTurn,
			   printBoard,
			   getPlayerInput,
			   gameLoop,
			   undo, 
			   redo,
			   main) where 


import GameLogic 
import Control.Monad 
import Control.Monad.Trans.State 
import Data.List hiding (ix)
import Text.Read (readMaybe)
import Control.Monad.IO.Class (liftIO)
import Control.Lens

makeLenses ''GameState

type TicTacToe a = StateT GameState IO a


handlePlayerTurn :: TicTacToe ()
handlePlayerTurn = do
  state <- get
  -- IO in shell
  liftIO $ putStrLn $ "Player " ++ show (state ^. currentPlayer) ++ "'s turn"
  printBoard  
  move <- getPlayerInput  

  case move of 
    Right msg -> case msg of 
      "undo" -> do undo 
      "redo" -> do redo 
      _ -> do 
        liftIO $ putStrLn "Invalid input"
        handlePlayerTurn
    Left move' -> case processMove state move' of
          StateUpdate newState -> put newState
          InvalidMove msg -> do
            liftIO $ putStrLn msg
            handlePlayerTurn  
          GameAlreadyOver msg -> do
            liftIO $ putStrLn msg
    _ -> do 
      liftIO $ putStrLn "Invalid input"
      handlePlayerTurn
      


printBoard :: TicTacToe ()
printBoard = do 
  state <- get 
  liftIO $ putStrLn $ renderBoard' (state ^. board)
  where
    renderBoard' :: Board -> String
    renderBoard' board = 
      let rows = map renderRow board
      in unlines (intersperse "---------" rows)
      where
        renderRow = unwords . map cellToString
        cellToString Nothing = " "
        cellToString (Just X) = "X"
        cellToString (Just O) = "O"


getPlayerInput :: TicTacToe (Either (Int, Int) String)
getPlayerInput = do
  liftIO $ putStrLn "Enter row and column (0-2): "
  input <- liftIO $ getLine
  let parts = words input
  case parts of
    ["redo"] -> return $ Right "redo"
    ["undo"] -> return $ Right "undo"
    [rowStr, colStr] -> case (readMaybe rowStr, readMaybe colStr) of
      (Just row, Just col) -> return $ Left (row, col)
      _ -> do
        liftIO $ putStrLn "Please enter two integers"
        getPlayerInput
    _ -> do
      liftIO $ putStrLn "Please enter two numbers separated by space"
      getPlayerInput


gameLoop :: TicTacToe ()
gameLoop = do
  state <- get
  if state ^. gameOver 
    then do
      let msg = getGameResultMessage state  
      liftIO $ putStrLn msg
    else do
      handlePlayerTurn
      gameLoop

undo :: TicTacToe ()
undo = do 
  state <- get 
  case state ^. undoStack of 
    [] -> liftIO $ putStrLn "Nothing to undo"
    (previous : rest) -> do 
      put $ previous & undoStack .~ rest
                     & redoStack %~ (state :)
      liftIO $ putStrLn "Undid last move."

redo :: TicTacToe ()
redo = do 
  state <- get 
  case state ^. redoStack of 
    [] -> liftIO $ putStrLn "Nothing to redo"
    (next : rest) -> do 
      put $ next & undoStack %~ (state :)
                 & redoStack .~ rest
      liftIO $ putStrLn "Redid Move"

runGame :: IO ((), GameState)
runGame = runStateT gameLoop initialState

main :: IO ()
main = do
  (_, finalState) <- runGame
  putStrLn "Game Finished!"




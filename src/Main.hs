module Main where

import TicTacToe
import System.IO

-- |Greet user, start recursion, and say farewell to user when game ends
main :: IO ()
main = do
    putStrLn "Welcome to tic tac toe."
    putStrLn ""
    typeGame <- promptLine "Do you want play with 1 or 2 players?"
    if typeGame `elem` ["1","2"]
        then 
            if typeGame == "1"
                then do
                    putStrLn ""
                    symbol <- getSymbol

                    if symbol == "X" 
						then playerXMove emptyBoard typeGame
					else playerOMove emptyBoard typeGame
            else
                playerXMove emptyBoard typeGame --start recursion
        else do 
            putStrLn "Hmm i dont undestand. Please enter 1 or 2"
            putStrLn ""
            main


getSymbol :: IO String
getSymbol = do
    symbol <- promptLine "Choose your symbol (X or O)"
    if symbol `elem` ["X","O"]
        then
            return $ symbol
    else do
        putStrLn "Hmm i dont undestand."
        putStrLn ""
        getSymbol
    
-- |Helper function to ensure prompt is made before user is expected to input something
promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

-- |Code common to both player and computer at the end of the game
endRecursion :: Board -> String -> IO ()
endRecursion b typeGame = do
    putStrLn (show b)
    putStrLn (winner b)
    restartGame typeGame
-- end recursion


-- |Grab the user's move, and feed that to tic-tac-toe. Recurse as needed.
playerXMove :: Board -> String -> IO ()
playerXMove board typeGame = do
    putStrLn (show board)
    loc <- promptLine "Where do you want to place your X? "
    putStrLn ""
    let moveLoc = Left (read loc) 
    let newBoard = findAndReplace board moveLoc (Right X)
    if won newBoard || draw newBoard
        then endRecursion newBoard typeGame
        else if elem moveLoc (possibleMoves board)
			then do
				if typeGame == "1" then do
					compOMove newBoard typeGame        -- continue recursion
				else 
					playerOMove newBoard typeGame
		else do
			putStrLn "Invalid Move! Please Try again! "
			putStrLn ""
			playerXMove newBoard typeGame


playerOMove :: Board -> String -> IO ()
playerOMove board typeGame = do
    putStrLn (show board)
    loc <- promptLine "Where do you want to place your O? "
    putStrLn ""
    let moveLoc = Left (read loc) 
    let newBoard = findAndReplace board moveLoc (Right O)
    if won newBoard || draw newBoard
        then endRecursion newBoard typeGame
        else if elem moveLoc (possibleMoves board)
			then do
				if typeGame == "1" then do
					compXMove newBoard typeGame        -- continue recursion
				else do
					playerXMove newBoard typeGame
		else do
			putStrLn "Invalid Move! Please Try again! "
			putStrLn ""
			playerOMove newBoard typeGame

                
-- |Make a decision based on the board on where to move. Recurse as needed.
compXMove :: Board -> String -> IO ()
compXMove board typeGame = do
    let newBoard = makeXMove board
    if won newBoard || draw newBoard
        then endRecursion newBoard typeGame    -- end recursion
        else playerOMove newBoard typeGame    -- continue recursion

-- |Make a decision based on the board on where to move. Recurse as needed.
compOMove :: Board -> String -> IO ()
compOMove board typeGame = do
    let newBoard = makeOMove board
    if won newBoard || draw newBoard
        then endRecursion newBoard typeGame    -- end recursion
        else playerXMove newBoard typeGame    -- continue recursion

restartGame :: String -> IO()
restartGame typeGame = do
  putStrLn "Would you like to play again? (y/n)"
  newGame <- getLine
  if newGame == "y" 
    then do main
      else if newGame == "n" then
        putStrLn "Thanks for playing!"
      else do
        putStrLn "Hmm i dont undestand. Please enter 'y' or 'n'"
        restartGame typeGame
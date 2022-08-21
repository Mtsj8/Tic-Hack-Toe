module Main where

import TicTacToe
import System.IO

-- |Greet user, start recursion, and say farewell to user when game ends
main :: IO ()
main = do
	putStrLn "Welcome to tic tac toe."
	typeGame <- promptLine "Do you want play with 1 or 2 players?"
		--start recursion
	player1Move emptyBoard typeGame

	
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
	restart typeGame
-- end recursion


-- |Grab the user's move, and feed that to tic-tac-toe. Recurse as needed.
player1Move :: Board -> String -> IO ()
player1Move board typeGame = do
	putStrLn (show board)
	loc <- promptLine "Where do you want to place your X? "
	putStrLn ""
	let moveLoc = Left (read loc) 
	let newBoard = findAndReplace board moveLoc (Right X)
	if won newBoard || draw newBoard
		then endRecursion newBoard typeGame
		else if typeGame == "1"
			then compMove newBoard	typeGame		-- continue recursion
			else if elem moveLoc (possibleMoves board) 
				then player2Move newBoard typeGame
				else do
				putStrLn "Invalid Move! Please Try again! "
				putStrLn ""
				player1Move newBoard typeGame


player2Move :: Board -> String -> IO ()
player2Move board typeGame = do
	putStrLn (show board)
	loc <- promptLine "Where do you want to place your O? "
	putStrLn ""
	let moveLoc = Left (read loc) 
	let newBoard = findAndReplace board moveLoc (Right O)
	if won newBoard || draw newBoard
		then endRecursion newBoard typeGame
		else if typeGame == "1"
			then compMove newBoard typeGame		-- continue recursion
			else if elem moveLoc (possibleMoves board) 
				then player1Move newBoard typeGame
				else do
				putStrLn "Invalid Move! Please Try again! "
				putStrLn ""
				player2Move newBoard typeGame

				
-- |Make a decision based on the board on where to move. Recurse as needed.
compMove :: Board -> String -> IO ()
compMove board typeGame = do
	let newBoard = makeOMove board
	if won newBoard || draw newBoard
		then endRecursion newBoard typeGame	-- end recursion
		else player2Move newBoard typeGame	-- continue recursion

restart :: String -> IO()
restart typeGame = do
  putStrLn "Would you like to play again? (y/n)"
  playAgain <- getLine
  if playAgain == "y" 
	then do main
  	else if playAgain == "n" then
		putStrLn "Thanks for playing!"
  	else do
    	putStrLn "Hmm i dont undestand. Please enter 'y' or 'n'"
    	restart typeGame
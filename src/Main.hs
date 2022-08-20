module Main where

import TicTacToe
import System.IO

-- |Greet user, start recursion, and say farewell to user when game ends
main :: IO ()
main = do
	putStrLn "Welcome to tic tac toe."

	--start recursion
	playerMove emptyBoard
	
	putStrLn "Thanks for playing!"

-- |Helper function to ensure prompt is made before user is expected to input something
promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

-- |Code common to both player and computer at the end of the game
endRecursion :: Board -> IO ()
endRecursion b = do
	putStrLn (show b)
	putStrLn (winner b)
	restart
-- end recursion

-- |Grab the user's move, and feed that to tic-tac-toe. Recurse as needed.
playerMove :: Board -> IO ()
playerMove board = do
	putStrLn (show board)
	loc <- promptLine "Where do you want to place your X? "
	putStrLn ""
	let moveLoc = Left (read loc)
	let newBoard = findAndReplace board moveLoc (Right X)
	if won newBoard || draw newBoard
		then endRecursion newBoard
		else if loc `elem` ["1","2","3","4","5","6","7","8","9"]
			then compMove newBoard			-- continue recursion
			else 
				putStrLn "Invalid Move"

-- |Make a decision based on the board on where to move. Recurse as needed.
compMove :: Board -> IO ()
compMove board = do
	let newBoard = makeOMove board
	if won newBoard || draw newBoard
		then endRecursion newBoard 	-- end recursion
		else playerMove newBoard 	-- continue recursion

restart :: IO()
restart = do
  putStrLn "Would you like to play again? (y/n)"
  playAgain <- getLine
  if playAgain == "y" then do
    putStrLn "Welcome to tic tac toe."
    playerMove emptyBoard
  else if playAgain == "n" then
	putStrLn "Thanks for playing!"
  else do
    putStrLn "Hmm i dont undestand. Please enter 'y' or 'n'"
    restart
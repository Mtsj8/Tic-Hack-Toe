module Main where

import TicTacToe
import System.IO

main :: IO ()
main = do
    putStrLn "Bem Vindo ao Jogo da Velha em Haskell"
    putStrLn ""
    typeGame <- promptLine "Você gostaria de jogar com 1 ou 2 jogadores? (Digite 1 ou 2) "
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
                playerXMove emptyBoard typeGame 
        else do 
            putStrLn "Hmm, desculpe não entendi. Escolha a opção 1 ou 2."
            putStrLn ""
            main


getSymbol :: IO String
getSymbol = do
    symbol <- promptLine "Escolha o simbolo que gostaria de jogar (Digite X ou O) "
    if symbol `elem` ["X","O"]
        then
            return $ symbol
    else do
        putStrLn "Hmm, desculpe não entendi. Escolha a opção X ou O"
        putStrLn ""
        getSymbol
    
promptLine :: String -> IO String
promptLine text = do
    putStr text
    hFlush stdout
    getLine

endRecursion :: Board -> String -> IO ()
endRecursion b typeGame = do
    putStrLn (show b)
    putStrLn (winner b)
    restartGame typeGame


playerXMove :: Board -> String -> IO ()
playerXMove board typeGame = do
    putStrLn (show board)
    loc <- promptLine "Aonde você gostaria de colocar o X? "
    putStrLn ""
    let moveLoc = Left (read loc) 
    let newBoard = replacePiece board moveLoc (Right X)
    if haveWon newBoard || draw newBoard
        then endRecursion newBoard typeGame
        else if elem moveLoc (viableMoves board)
			then do
				if typeGame == "1" then do
					compOMove newBoard typeGame    
				else 
					playerOMove newBoard typeGame
		else do
			putStrLn "Movimento Inválido, tente de novo!"
			putStrLn ""
			playerXMove newBoard typeGame


playerOMove :: Board -> String -> IO ()
playerOMove board typeGame = do
    putStrLn (show board)
    loc <- promptLine "Aonde você gostaria de colocar o O? "
    putStrLn ""
    let moveLoc = Left (read loc) 
    let newBoard = replacePiece board moveLoc (Right O)
    if haveWon newBoard || draw newBoard
        then endRecursion newBoard typeGame
        else if elem moveLoc (viableMoves board)
			then do
				if typeGame == "1" then do
					compXMove newBoard typeGame     
				else do
					playerXMove newBoard typeGame
		else do
			putStrLn "Movimento Inválido, tente de novo!"
			putStrLn ""
			playerOMove newBoard typeGame

                
compXMove :: Board -> String -> IO ()
compXMove board typeGame = do
    let newBoard = makeXMove board
    if haveWon newBoard || draw newBoard
        then endRecursion newBoard typeGame    
        else playerOMove newBoard typeGame   

compOMove :: Board -> String -> IO ()
compOMove board typeGame = do
    let newBoard = makeOMove board
    if haveWon newBoard || draw newBoard
        then endRecursion newBoard typeGame    
        else playerXMove newBoard typeGame    

restartGame :: String -> IO()
restartGame typeGame = do
  putStrLn "Você gostaria de jogas mais uma vez? (S ou N)"
  newGame <- getLine
  if newGame == "S" 
    then do main
      else if newGame == "N" then
        putStrLn "Obrigada por jogar!"
      else do
        putStrLn "Hmm, desculpe não entendi. Escolha a opção S ou N"
        restartGame typeGame
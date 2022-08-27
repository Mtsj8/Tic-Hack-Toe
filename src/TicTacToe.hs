module TicTacToe where

{-# LANGUAGE FlexibleInstances #-}


import System.Random
import Data.Either
import Safe
import Data.Maybe
import Data.List

data Symbol = X | O
    deriving (Show, Eq)

type Piece = Either Int Symbol

type Three = [Piece]

data Board = Board Three Three Three
    deriving (Eq)

-- Define board
instance Show Board where
    show board =
        unlines . arround "+----++---++----+" . map (concat . arround "||". map element) $ (rows board)
        where
            arround x xs = concat [[x], intersperse x xs , [x]]
            element = either (\n -> " " ++ show n ++ " ") (\n -> pieceColor n)

-- Define cor de cada simbolo
pieceColor :: Symbol -> String 
pieceColor s
        | s == X     = concat [esc 35," ", show s," ", esc 0]
        | otherwise  = concat [esc 36," ", show s," ", esc 0]
    where 
        esc i = concat ["\ESC[", show i, "m"]

-- Define board vazio, ou, do incio do jogo
emptyBoard :: Board
emptyBoard = Board [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

-- Define as linhas do board
rows :: Board -> [Three]
rows (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [x, y, z]

-- Define as colunas do board
cols :: Board -> [Three]
cols (Board [a, b, c] [d, e, f] [g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

-- Define as diagonais do board
diagons :: Board -> [Three]
diagons (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, e, i], [c, e, g]]

-- Buscas jogadas viaveis
viableMoves :: Board -> [Piece]
viableMoves board = filter isLeft (boardList board)

-- Verifica se jogo tem vencedor
haveWon :: Board -> Bool
haveWon b = foldl (\acc curr -> acc || (fullBoard curr)) False ((rows b) ++ (cols b) ++ (diagons b))
    where fullBoard ts@[a,b,c] = foldl (\acc curr -> acc && (isRight curr)) True ts && a == b && b == c

-- Verifica se jogo esta empatado
draw :: Board -> Bool
draw b = length (viableMoves b) == 0

-- Define o vencedor
winner :: Board -> String 
winner b = if length winnerType > 0 then head winnerType else "É um empate!!"
    where
        allConfigs = ((rows b) ++ (cols b) ++ (diagons b))
        fullBoard ts@[a,b,c] = foldl (\acc curr -> acc && (isRight curr)) True ts && a == b && b == c
        winnerType = [if a == (Right O) then "Jogador"++ pieceColor O ++ "venceu!" else "Jogador"++ pieceColor X ++ "venceu!" | curr@[a,b,c] <- allConfigs, fullBoard curr]

-- Board para lista
boardList :: Board -> [Piece]
boardList (Board x y z) = concat [x, y, z]

-- Lista para board
listBoard :: [Piece] -> Board
listBoard [a,b,c,d,e,f,g,h,i] = Board [a,b,c] [d,e,f] [g,h,i]

-- Substitui local do tabuleiro por simbolo
replacePiece :: Board -> Piece -> Piece -> Board
replacePiece board p1 p2 = listBoard [place x | x <- bl]
    where
        place x =  if x==p1 then p2 else x
        bl = boardList board

-- Jogada aleatoria da IA      
seed::Int
seed = 42
generator = mkStdGen seed

getRandomElement :: [Piece] -> Piece
getRandomElement pieces = pieces !! rand where
    n = length pieces
    (rand, _) = randomR (0,(n-1)) generator

-- Realiza jogada do simbolo O
makeOMove :: Board -> Board
makeOMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
    | elem e (viableMoves board) = replacePiece board e (Right O)
    | otherwise                     = if length (viableMoves board) > 0
        then replacePiece board (getRandomElement (viableMoves board)) (Right O)
        else board 

-- Realiza jogada do simbolo X
makeXMove :: Board -> Board
makeXMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
    | elem e (viableMoves board) = replacePiece board e (Right X)
    | otherwise                     = if length (viableMoves board) > 0
        then replacePiece board (getRandomElement (viableMoves board)) (Right X)
        else board 


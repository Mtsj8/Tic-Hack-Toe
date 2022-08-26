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

instance Show Board where
    show board =
        unlines . arround "+----++---++----+" . map (concat . arround "||". map element) $ (rows board)
        where
            arround x xs = concat [[x], intersperse x xs , [x]]
            element = either (\n -> " " ++ show n ++ " ") (\n -> color n)


color :: Symbol -> String 
color s
        | s == X     = concat [esc 35," ", show s," ", esc 0]
        | otherwise  = concat [esc 36," ", show s," ", esc 0]
    where 
        esc i = concat ["\ESC[", show i, "m"]

emptyBoard :: Board
emptyBoard = Board [Left 1, Left 2, Left 3] [Left 4, Left 5, Left 6] [Left 7, Left 8, Left 9]

rows :: Board -> [Three]
rows (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [x, y, z]

cols :: Board -> [Three]
cols (Board [a, b, c] [d, e, f] [g, h, i]) = [[a, d, g], [b, e, h], [c, f, i]]

diagons :: Board -> [Three]
diagons (Board x@[a, b, c] y@[d, e, f] z@[g, h, i]) = [[a, e, i], [c, e, g]]


full :: Three -> Bool
full ts@[a,b,c] = withoutLeft && equals
    where
        withoutLeft = foldl (\acc curr -> acc && (isRight curr)) True ts
        equals = a == b && b == c

haveWon :: Board -> Bool
haveWon b = foldl (\acc curr -> acc || (full curr)) False ((rows b) ++ (cols b) ++ (diagons b))

draw :: Board -> Bool
draw b = length (possibleMoves b) == 0

winner :: Board -> String 
winner b = if length winnerType > 0 then head winnerType else "É um empate!!"
    where
        allConfigs = ((rows b) ++ (cols b) ++ (diagons b))
        winnerType = [if a == (Right O) then "Jogador"++ color O ++ "venceu!" else "Jogador"++ color X ++ "venceu!" | curr@[a,b,c] <- allConfigs, full curr]


possibleMoves :: Board -> [Piece]
possibleMoves board = filter isLeft (boardToList board)

boardToList :: Board -> [Piece]
boardToList (Board x y z) = x ++ y ++ z

listToBoard :: [Piece] -> Board
listToBoard [a,b,c,d,e,f,g,h,i] = Board [a,b,c] [d,e,f] [g,h,i]

findAndReplace :: Board -> Piece -> Piece -> Board
findAndReplace board p1 p2 = listToBoard [if x==p1 then p2 else x | x <- bl]
    where bl = boardToList board
        

seed::Int
seed = 42
generator = mkStdGen seed

getRandomElement :: [Piece] -> Piece
getRandomElement pieces = pieces !! rand where
    n = length pieces
    (rand, _) = randomR (0,(n-1)) generator

makeOMove :: Board -> Board
makeOMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
    | elem e (possibleMoves board) = findAndReplace board e (Right O)
    | otherwise                     = if length (possibleMoves board) > 0
        then findAndReplace board (getRandomElement (possibleMoves board)) (Right O)
        else board 

makeXMove :: Board -> Board
makeXMove board@(Board x@[a, b, c] y@[d, e, f] z@[g, h, i])
    | elem e (possibleMoves board) = findAndReplace board e (Right X)
    | otherwise                     = if length (possibleMoves board) > 0
        then findAndReplace board (getRandomElement (possibleMoves board)) (Right X)
        else board 


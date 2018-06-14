module Game where

import Board

data Player = PlayerFox | PlayerHounds
    deriving (Show, Eq, Enum)

-- Check if player has any moves
allMoves :: Board -> Player -> [(Piece, Direction)] -- TODO change to Move instead of tuple
allMoves board PlayerFox =
    possibleDirections board Fox

allMoves board PlayerHounds =
    (possibleDirections board Hound1)  ++ (possibleDirections board Hound2)
        ++ (possibleDirections board Hound3) ++ (possibleDirections board Hound4)


-- Get board winner or nothing.
boardWinner :: Board -> Player -> Maybe Player
boardWinner (Board (Position _ 7) _) _ = Just PlayerFox
boardWinner board turn
    | (allMoves board PlayerFox) == [] = Just PlayerHounds
    | turn == PlayerHounds && (allMoves board PlayerHounds) == [] = Just PlayerFox
    | otherwise = Nothing

data State = Turn Player | Winner Player
    deriving (Show, Eq)

data Game = Game Board State
    deriving (Show)

initGame :: Game
initGame = Game initBoard $ Turn PlayerFox

nextState :: Game -> State
nextState (Game _ (Winner w)) = Winner w
nextState (Game b (Turn t)) =
    case boardWinner b (next t) of
        Just p -> Winner p
        Nothing -> Turn $ next t
    where next PlayerFox    = PlayerHounds
          next PlayerHounds = PlayerFox

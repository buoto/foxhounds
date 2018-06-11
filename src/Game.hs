module Game where

import Board

data Player = PlayerFox | PlayerHounds
    deriving (Show, Eq, Enum)

-- Check if player has any moves
hasAnyMoves :: Board -> Player -> Bool
hasAnyMoves board PlayerFox =
    length (possibleDirections board Fox) /= 0

hasAnyMoves board PlayerHounds =
    length (possibleDirections board Hound1) /= 0 || length (possibleDirections board Hound2) /= 0
        || length (possibleDirections board Hound4) /= 0 || length (possibleDirections board Hound4) /= 0


-- Get board winner or nothing.
boardWinner :: Board -> Player -> Maybe Player
boardWinner (Board (Position _ 7) _) _ = Just PlayerFox
boardWinner board turn
    | not(hasAnyMoves board PlayerFox) = Just PlayerHounds
    | turn == PlayerHounds && not(hasAnyMoves board PlayerHounds) = Just PlayerFox
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
    case boardWinner b t of
        Just p -> Winner p
        Nothing -> Turn $ next t
    where next PlayerFox    = PlayerHounds
          next PlayerHounds = PlayerFox

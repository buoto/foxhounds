module Game where

import Board

data Player = PlayerFox | PlayerHounds
    deriving (Show, Eq, Enum)

-- Get board winner or nothing.
boardWinner :: Board -> Maybe Player
boardWinner (Board (Position _ 7) _) = Just PlayerFox
boardWinner b =
    case possibleDirections b Fox of
        [] -> Just PlayerHounds
        _  -> Nothing

data State = Turn Player | Winner Player
    deriving (Show, Eq)

data Game = Game Board State
    deriving (Show)

initGame :: Game
initGame = Game initBoard (Turn PlayerFox)

nextState :: Game -> State
nextState (Game _ (Winner w)) = (Winner w)
nextState (Game b (Turn t)) =
    case boardWinner b of
        Just p -> Winner p
        Nothing -> Turn (next t)
    where next PlayerFox    = PlayerHounds
          next PlayerHounds = PlayerFox

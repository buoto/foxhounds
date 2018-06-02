module Board where

type Coord = Int

data Position = Position {x :: Int, y :: Int}
    deriving (Eq)

data Board = Board { fox :: Position, hounds :: [Position] }

instance Show Board where
    show = showBoard

concWithNewline :: String -> String -> String
concWithNewline = ((++) . (++ "\n"))

showBoard :: Board -> String
showBoard b = foldr concWithNewline "" [showBoardRow b rnum | rnum <- [7, 6..0]]

showBoardRow :: Board -> Int -> String
showBoardRow b rnum = [showBoardCell b (Position cnum rnum) | cnum <- [0..7]]

showBoardCell :: Board -> Position -> Char
showBoardCell (Board fox hounds) pos
    | pos == fox = 'F'
    | elem pos hounds = 'H'
    | otherwise = '.'



initBoard :: Board
initBoard = Board (Position 0 0) [ (Position 1 7)
                                 , (Position 3 7)
                                 , (Position 5 7)
                                 , (Position 7 7)
                                 ]


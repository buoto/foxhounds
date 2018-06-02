module Board where

type Coord = Int

data Position = Position {x :: Int, y :: Int}
    deriving (Eq, Show)

data Board = Board { fox :: Position, hounds :: (Position, Position, Position, Position) }
    deriving (Eq)

instance Show Board where
    show = showBoard

initBoard :: Board
initBoard = Board (Position 0 0) ( (Position 1 7)
                                 , (Position 3 7)
                                 , (Position 5 7)
                                 , (Position 7 7)
                                 )

showBoard :: Board -> String
showBoard b = foldr concWithNewline "" [showBoardRow b rnum | rnum <- [7, 6..0]]

concWithNewline :: String -> String -> String
concWithNewline = ((++) . (++ "\n"))

showBoardRow :: Board -> Int -> String
showBoardRow b rnum = [showBoardCell b (Position cnum rnum) | cnum <- [0..7]]

showBoardCell :: Board -> Position -> Char
showBoardCell (Board fox hounds) pos
    | pos == fox = 'F'
    | containsTuple4 hounds pos = 'H'
    | otherwise = '.'

containsTuple4 (p1, p2, p3, p4) pos = p1 == pos || p2 == pos || p3 == pos || p4 == pos

data Piece = Fox | Hound1 | Hound2 | Hound3 | Hound4

data Direction = North | East | South | West

data Move = Move Piece Direction

applyMove :: Move -> Board -> Board
applyMove (Move Fox dir) (Board fox hounds) = Board (movePiece dir fox) hounds
applyMove (Move piece dir) (Board fox (h1, h2, h3, h4)) =
    case piece of
        Hound1 -> Board fox ((move h1), h2, h3, h4)
        Hound2 -> Board fox (h1, (move h2), h3, h4)
        Hound3 -> Board fox (h1, h2, (move h3), h4)
        Hound4 -> Board fox (h1, h2, h3, (move h4))
    where move = movePiece dir

movePiece :: Direction -> Position -> Position
movePiece dir (Position x y) =
    case dir of
        North -> Position x    (y+1)
        East  -> Position (x+1) y
        South -> Position x    (y-1)
        West  -> Position (x-1) y

-- Get possible moves for piece for provided board state.
possibleDirections :: Board -> Piece -> [Direction]
possibleDirections _ _ = [North, East, South, West] -- TODO

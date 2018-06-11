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

data Direction = NE | SE | NW | SW

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
        NE -> Position (x+1) (y+1)
        SE  -> Position (x+1) (y-1)
        NW -> Position (x-1) (y+1)
        SW  -> Position (x-1) (y-1)


isPositionEmpty :: [Position] -> Position -> Bool
isPositionEmpty [] _ = True
isPositionEmpty (boardPosition:tail) lookPosition | (x lookPosition < 0 || y lookPosition < 0 || x lookPosition > 7 || y lookPosition > 7) = False
                                                  | boardPosition == lookPosition = False
                                                  | otherwise = isPositionEmpty tail lookPosition

accumulatedDirections :: [Position] -> [Position] -> [Position]
accumulatedDirections _ [] = []
accumulatedDirections boardPositions (lookPosition:tail)
    | (isPositionEmpty boardPositions lookPosition) = lookPosition:(accumulatedDirections boardPositions tail)
    | otherwise = (accumulatedDirections boardPositions tail)

-- Get possible moves for piece for provided board state.
possibleDirections :: Board -> Piece -> [Position]
possibleDirections (Board fox (h1, h2, h3, h4)) Fox =
    accumulatedDirections [fox, h1, h2, h3, h4] [(movePiece NE fox), (movePiece SE fox), (movePiece NW fox), (movePiece SW fox)]

possibleDirections (Board fox (h1, h2, h3, h4)) piece =
    case piece of
        Hound1 -> accumulatedDirections [fox, h1, h2, h3, h4] [(movePiece SE h1), (movePiece SW h1)]
        Hound2 -> accumulatedDirections [fox, h1, h2, h3, h4] [(movePiece SE h2), (movePiece SW h2)]
        Hound3 -> accumulatedDirections [fox, h1, h2, h3, h4] [(movePiece SE h3), (movePiece SW h3)]
        Hound4 -> accumulatedDirections [fox, h1, h2, h3, h4] [(movePiece SE h4), (movePiece SW h4)]

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
    deriving (Eq, Show)

data Direction = NE | SE | NW | SW
    deriving (Eq, Show)


data Move = Move Piece Direction

applyMove :: Board -> Move -> Board
applyMove (Board fox hounds) (Move Fox dir) = Board (movePiece dir fox) hounds
applyMove (Board fox (h1, h2, h3, h4)) (Move piece dir) =
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

accumulatedDirections :: [Position] -> [Position] -> [Direction] -> Piece -> [(Piece, Direction)] -- TODO change to Move instead of tuple
accumulatedDirections _ [] _ _ = []
accumulatedDirections boardPositions (lookPosition:tailPosition) (lookDirection:tailDirection) piece
    | (isPositionEmpty boardPositions lookPosition) = (piece, lookDirection):(accumulatedDirections boardPositions tailPosition tailDirection piece)
    | otherwise = (accumulatedDirections boardPositions tailPosition tailDirection piece)

-- Get possible moves for piece for provided board state.
possibleDirections :: Board -> Piece -> [(Piece, Direction)] -- TODO change to Move instead of tuple
possibleDirections (Board fox (h1, h2, h3, h4)) Fox =
    accumulatedDirections
        [fox, h1, h2, h3, h4]
        [(movePiece NE fox), (movePiece SE fox), (movePiece NW fox), (movePiece SW fox)]
        [NE, SE, NW, SW]
        Fox

possibleDirections (Board fox (h1, h2, h3, h4)) piece =
    accumulatedDirections
        [fox, h1, h2, h3, h4]
        [(movePiece SE x), (movePiece SW x)]
        [SE, SW]
        piece
    where
        x = case piece of
            Hound1 -> h1
            Hound2 -> h2
            Hound3 -> h3
            Hound4 -> h4


isMoveLegal :: Move -> Board -> Bool
isMoveLegal (Move piece direction) board = elem (piece, direction) $ possibleDirections board piece

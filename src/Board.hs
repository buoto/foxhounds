module Board where

data Piece = Fox | Hound

data Position = Position {x :: Int, y :: Int}

type PieceOnBoard = (Piece, Position)

data Board = Board { fox :: PieceOnBoard, hounds :: [PieceOnBoard] }

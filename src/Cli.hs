module Cli
    ( cliLoop
    ) where

import System.Exit
import Board
import Game
import Tree

cliLoop :: IO ()
cliLoop = do
    input <- promptLine "Choose option (h for help):"
    handleOption input
    cliLoop

handleOption :: Char -> IO ()

handleOption 'h' = do
    putStrLn "Available options:"
    putStrLn "s - Start new game"
    putStrLn "h - Show this help"
    putStrLn "q - Quit"

handleOption 's' = runGameCli initGame

handleOption 'q' = exitSuccess

handleOption _ = putStrLn "Unknown unput!"

promptLine :: String -> IO Char
promptLine prompt = do
    putStrLn prompt
    line <- getLine
    case line of
        (c:_) -> return c
        [] -> return 'h'

runGameCli :: Game -> IO ()
runGameCli (Game board (Winner player)) = putStr $ show player ++ " has won!"

runGameCli (Game board (Turn PlayerFox)) = runGameCli (Game nextBoard (nextState afterMove))
  where (value, nextBoard) = pickBestBoard board PlayerFox
        afterMove = Game nextBoard (Turn PlayerFox)

runGameCli (Game board (Turn PlayerHounds)) = do
    putStr $ showBoard board
    putStr $ show $ allMoves board PlayerHounds
    nextBoard <- applyMove board <$> promptTurn board
    runGameCli (Game nextBoard (nextState (Game nextBoard (Turn PlayerHounds))))

promptTurn :: Board -> IO Move
promptTurn board = do
  hound <-  houndFromCommand <$> promptLine gameMenu
  direction <- directionFromCommand <$> promptLine directionPrompt -- TODO print possible directions
  if isMoveLegal (Move hound direction) board then
    return $ Move hound direction
  else do
    putStrLn "Invalid move! Try again."
    promptTurn board

directionFromCommand :: Char->Direction
directionFromCommand _ = SW -- TODO


houndFromCommand :: Char -> Piece
houndFromCommand _ = Hound1 -- TODO

directionPrompt = "Check direction\n"

gameMenu = "Choose one of the following numbers:\n\
 \1 .. 4 - number of hound to move\n"

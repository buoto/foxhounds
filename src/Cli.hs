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

handleOption 'c' = return()

handleOption _ = putStrLn "Unknown unput!"

promptLine :: String -> IO Char
promptLine prompt = do
    putStrLn prompt
    line <- getLine
    case line of
        (c:_) -> return c
        [] -> return 'h'

runGameCli :: Game -> IO ()
runGameCli (Game board (Winner player)) = putStrLn $ show player ++ " has won!"

runGameCli (Game board (Turn PlayerFox)) = runGameCli (Game nextBoard (nextState afterMove))
  where (value, nextBoard) = pickBestBoard board PlayerFox
        afterMove = Game nextBoard (Turn PlayerFox)

runGameCli (Game board (Turn PlayerHounds)) = do
    putStr $ showBoard board
    putStrLn $ show $ allMoves board PlayerHounds
    alternativeBoardMaybe <- promptOperation board
    case alternativeBoardMaybe of
      Just alternativeBoard -> do
        nextBoard <- applyMove alternativeBoard <$> promptTurn alternativeBoard
        runGameCli (Game nextBoard (nextState (Game nextBoard (Turn PlayerHounds))))
      Nothing -> return ()

promptOperation :: Board -> IO (Maybe Board)
promptOperation board= do
  line <- promptLine "c-continue, s-save, l-load, q-exit, b-start from beggining"
  case line of
    'c' -> return (Just board)
    's' -> do
        writeFile "save.txt" (dumpBoard board)
        return $ Just board  -- TODO save
    'l' -> do
        newBoard <- loadBoard <$> readFile "save.txt"-- TODO load board
        putStrLn $ show newBoard
        return $ Just board
    'q' -> return Nothing
    'b' -> do
        putStrLn $ show initBoard
        return (Just initBoard)

promptTurn :: Board -> IO Move
promptTurn board = do
  hound <- promptHound
  direction <-  promptDirection
  if isMoveLegal (Move hound direction) board then
    return $ Move hound direction
  else do
    putStrLn "Invalid move! Try again."
    promptTurn board


promptHound :: IO Piece
promptHound = do
   maybeHound<- houndFromCommand <$> promptLine gameMenu
   case maybeHound of
     Just hound -> return hound
     Nothing    -> do
       putStrLn "Invalid input! Try again."
       promptHound

promptDirection :: IO Direction
promptDirection = do
  maybeDirection <- directionFromCommand <$> promptLine directionPrompt -- TODO print possible directions
  case maybeDirection of
    Just direction -> return direction
    Nothing        -> do
      putStrLn "Invalid input! Try again."
      promptDirection

directionFromCommand :: Char -> Maybe Direction
directionFromCommand '1' = Just SE
directionFromCommand '2' = Just SW
directionFromCommand 'e' = Just SE
directionFromCommand 'w' = Just SW
directionFromCommand 'r' = Just SE
directionFromCommand 'l' = Just SW
directionFromCommand  _  = Nothing

houndFromCommand :: Char -> Maybe Piece
houndFromCommand '1' = Just Hound1
houndFromCommand '2' = Just Hound2
houndFromCommand '3' = Just Hound3
houndFromCommand '4' = Just Hound4
houndFromCommand  _  = Nothing

directionPrompt = "Choose direction (l,r)"

gameMenu = "Choose one of the following numbers:\n\
 \1 .. 4 - number of hound to move"

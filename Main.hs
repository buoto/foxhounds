module Main where

import System.Exit

main :: IO ()
main = cliLoop

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

handleOption 's' = return ()

handleOption 'q' = exitSuccess

handleOption _ = putStrLn "Unknown unput!"

promptLine :: String -> IO Char
promptLine prompt = do
    putStr prompt
    head <$> getLine

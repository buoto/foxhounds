module Tree where
import Game
import Board
import Data.List

test :: Integer -> Integer
test x = 3

horizontalDistanceBetweenPositions :: Position -> Position -> Float
horizontalDistanceBetweenPositions pos1 pos2 = fromIntegral(abs(y pos2 - y pos1))

diagonalDistanceBetweenPositions :: Position -> Position -> Float
diagonalDistanceBetweenPositions pos1 pos2 = sqrt (fromIntegral(abs(x pos2 - x pos1))^2 + fromIntegral(abs(y pos2 - y pos1))^2)

foxDistance :: Position -> [Position] -> Float
foxDistance fox hounds = sum (map (diagonalDistanceBetweenPositions fox) hounds)

maxHoundDistance :: [Position] -> Float
maxHoundDistance (hound1:restHounds) = maximum (map (horizontalDistanceBetweenPositions hound1) restHounds)

calcHeuristic :: Board -> Float
calcHeuristic (Board fox (h1,h2,h3,h4)) =
    maxDistanceBetweenHounds + distanceToWolf / 2
    where
        distanceToWolf = foxDistance fox [h1,h2,h3,h4]
        maxDistanceBetweenHounds = maximum (map maxHoundDistance (permutations [h1,h2,h3,h4]))



-- calculates heuristic - the smaller, the better for the hounds



rate :: Board -> Player -> Integer -> Float
rate board player depth = if depth > 0 then minmax else heuristic
    where
        minmax | winner == Just player = 1000000
               | winner /= Nothing && winner /= Just player = -1000000
               | player == PlayerFox = maximum (map (\x -> (rate x (next player) (depth-1) )) (allPossibleBoards board player))
               | player == PlayerHounds = minimum (map (\x -> (rate x (next player) (depth-1) )) (allPossibleBoards board player))

        heuristic = calcHeuristic board * heuristicMultiplier

        heuristicMultiplier = if player == PlayerFox then 1 else -1

        winner = boardWinner board player
        next PlayerFox    = PlayerHounds
        next PlayerHounds = PlayerFox


allPossibleBoards :: Board -> Player -> [Board]
allPossibleBoards board player =
    map (\x -> applyMove (Move (fst x) (snd x)) board) allPossibleMoves
    where
        allPossibleMoves = allMoves board player


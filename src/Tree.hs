module Tree where
import Game
import Board
import Data.List

-- odleglosc miedzy pionkami w pionie
horizontalDistanceBetweenPositions :: Position -> Position -> Float
horizontalDistanceBetweenPositions pos1 pos2 = fromIntegral(abs(y pos2 - y pos1))

-- odleglosc miedzy pionkami w linii prostej
diagonalDistanceBetweenPositions :: Position -> Position -> Float
diagonalDistanceBetweenPositions pos1 pos2 = sqrt (fromIntegral(abs(x pos2 - x pos1))^2 + fromIntegral(abs(y pos2 - y pos1))^2)

-- suma odleglosci psow od lisa
foxDistance :: Position -> [Position] -> Float
foxDistance fox hounds = sum (map (diagonalDistanceBetweenPositions fox) hounds)

-- max z odleglosci psow od siebie w pionie
maxHoundDistance :: [Position] -> Float
maxHoundDistance (hound1:restHounds) = maximum (map (horizontalDistanceBetweenPositions hound1) restHounds)


-- heurystyka - odleglosc lisa do granicy na gorze * 10 + maksimum z odleglosci psow miedzy soba
--   w pionie + suma odleglosci psow do wilka / 2
-- im mniejsza, tym lepsza dla owiec
calcHeuristic :: Board -> Float
calcHeuristic (Board fox (h1,h2,h3,h4)) =
 foxDistanceToBorder * 10  + maxDistanceBetweenHounds + distanceToWolf / 2
    where
        distanceToWolf = foxDistance fox [h1,h2,h3,h4]
        maxDistanceBetweenHounds = maximum (map maxHoundDistance (permutations [h1,h2,h3,h4]))
        foxDistanceToBorder = fromIntegral $ 7 - (y fox)



-- pobranie planszy o najlepszej ocenie
getMaximumBoard :: [Board] -> [Float] -> (Float, Board)
getMaximumBoard [] [] = (-10000000000, (Board (Position 0 0) ( (Position 0 0), (Position 0 0), (Position 0 0), (Position 0 0))))
getMaximumBoard (b:boards) (r:rates) =
    if r > (fst otherMax) then (r, b) else otherMax
    where
        otherMax = getMaximumBoard boards rates

-- wybranie najlepszej planszy - min max
pickBestBoard :: Board -> Player -> (Float, Board)
pickBestBoard board player =
    getMaximumBoard possibleBoards rates
    where
        possibleBoards = allPossibleBoards board player
        rates = map (\x -> rate x (next player) 6) possibleBoards
        next PlayerFox = PlayerHounds
        next PlayerHounds = PlayerFox

-- drzewo min max
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
    map (\x -> applyMove board (Move (fst x) (snd x))) allPossibleMoves
    where
        allPossibleMoves = allMoves board player

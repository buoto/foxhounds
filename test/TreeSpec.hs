module TreeSpec where
import Tree
import Test.Hspec
import Game
import Board

spec = do
  describe "possibleMoves" $ do
    it "fox turn not blocked" $
        allPossibleBoards (Board (Position 1 1) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7)))
            PlayerFox `shouldBe` [
                (Board (Position 2 2) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7))),
                (Board (Position 2 0) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7))),
                (Board (Position 0 2) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7))),
                (Board (Position 0 0) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7)))
            ]
    it "fox in corner" $
        allPossibleBoards (Board (Position 0 0) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7)))
            PlayerFox `shouldBe` [
                (Board (Position 1 1) ( (Position 1 7), (Position 3 7), (Position 5 7), (Position 7 7)))
            ]
    it "no fox moves" $
        allPossibleBoards (Board (Position 0 0) ( (Position 1 1), (Position 3 7), (Position 5 7), (Position 7 7)))
                    PlayerFox `shouldBe` []
    it "hounds moves" $
        allPossibleBoards (Board (Position 5 3) ( (Position 1 7), (Position 4 4), (Position 5 7), (Position 7 7)))
            PlayerHounds `shouldBe` [
                (Board (Position 5 3) ( (Position 2 6), (Position 4 4), (Position 5 7), (Position 7 7))),
                (Board (Position 5 3) ( (Position 0 6), (Position 4 4), (Position 5 7), (Position 7 7))),
                (Board (Position 5 3) ( (Position 1 7), (Position 3 3), (Position 5 7), (Position 7 7))),
                (Board (Position 5 3) ( (Position 1 7), (Position 4 4), (Position 6 6), (Position 7 7))),
                (Board (Position 5 3) ( (Position 1 7), (Position 4 4), (Position 4 6), (Position 7 7))),
                (Board (Position 5 3) ( (Position 1 7), (Position 4 4), (Position 5 7), (Position 6 6)))
            ]
  describe "rate" $ do
    it "rate for winning board - fox" $
        rate (Board (Position 3 7) ( (Position 1 7), (Position 4 4), (Position 5 7), (Position 7 7))) PlayerFox 5
            `shouldBe` 1000000
    it "rate for winning board - hounds" $
        rate (Board (Position 4 4) ( (Position 5 5), (Position 5 3), (Position 3 3), (Position 3 5))) PlayerFox 5
            `shouldBe` -1000000
    it "rate: fox turn, depth 0" $
        rate (Board (Position 4 2) ( (Position 1 6), (Position 7 6), (Position 4 4), (Position 7 2))) PlayerFox 0
            `shouldBe` 11.5
    it "rate: hounds turn, depth 0" $
        rate (Board (Position 4 2) ( (Position 1 6), (Position 7 6), (Position 4 4), (Position 7 2))) PlayerHounds 0
            `shouldBe` -11.5
  describe "horizontalDistanceBetweenPositions" $ do
    it "bigger first" $
        horizontalDistanceBetweenPositions (Position 1 7) (Position 1 3) `shouldBe` 4
    it "smaller first" $
        horizontalDistanceBetweenPositions (Position 5 4) (Position 1 6) `shouldBe` 2
  describe "diagonalDistanceBetweenPositions" $ do
    it "classic 3x4x5" $
        diagonalDistanceBetweenPositions (Position 1 1) (Position 4 5) `shouldBe` 5
  describe "foxDistance" $ do
    it "2x classic + 2 + 3" $
        foxDistance (Position 4 2) [(Position 1 6), (Position 7 6), (Position 4 4), (Position 7 2)] `shouldBe` 15
  describe "maxHoundDistance" $ do
    it "2x classic + 2 + 3" $
        maxHoundDistance [(Position 1 6), (Position 7 6), (Position 4 4), (Position 7 2)] `shouldBe` 4
    it "the same row" $
        maxHoundDistance [(Position 1 6), (Position 7 6), (Position 4 6), (Position 2 6)] `shouldBe` 0
  describe "calcHeuristic" $ do
    it "2x classic + 2 + 3" $
        calcHeuristic (Board (Position 4 2) ( (Position 1 6), (Position 7 6), (Position 4 4), (Position 7 2))) `shouldBe` 11.5

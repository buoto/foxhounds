module BoardSpec where

import Test.Hspec
import Board

spec = do
  describe "showBoardCell" $ do
    it "prints fox" $
      showBoardCell initBoard (Position 0 0) `shouldBe` 'F'
    it "prints hound" $
      showBoardCell initBoard (Position 1 7) `shouldBe` 'H'
    it "prints empty" $
      showBoardCell initBoard (Position 0 1) `shouldBe` '.'
  describe "showBoardRow" $ do
    it "prints 7th initBoard row" $
        showBoardRow initBoard 7 `shouldBe` ".H.H.H.H"
  describe "showBoard" $ do
    it "prints initBoard" $
        showBoard initBoard `shouldBe` ".H.H.H.H\n........\n........\n........\n........\n........\n........\nF.......\n"
  describe "concWithNewline" $ do
    it "adds \\n inside" $
        concWithNewline "a" "b" `shouldBe` "a\nb"
  describe "movePiece" $ do
    it "moves NE" $
        movePiece NE (Position 1 1) `shouldBe` (Position 2 2)
    it "moves SE" $
        movePiece SE (Position 1 1) `shouldBe` (Position 2 0)
    it "moves NW" $
        movePiece NW (Position 1 1) `shouldBe` (Position 0 2)
    it "moves SW" $
        movePiece SW (Position 1 1) `shouldBe` (Position 0 0)
  describe "applyMove" $ do
    it "moves fox NE" $
        applyMove (Move Fox NE) initBoard  `shouldBe`  (Board (Position 1 1) ( (Position 1 7)
                                                                               , (Position 3 7)
                                                                               , (Position 5 7)
                                                                               , (Position 7 7)
                                                                               ))

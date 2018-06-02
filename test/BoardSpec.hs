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
    it "moves up" $
        movePiece North (Position 0 0) `shouldBe` (Position 0 1)
    it "moves down" $
        movePiece South (Position 0 1) `shouldBe` (Position 0 0)
    it "moves left" $
        movePiece West (Position 1 0) `shouldBe` (Position 0 0)
    it "moves right" $
        movePiece East (Position 0 0) `shouldBe` (Position 1 0)
  describe "applyMove" $ do
    it "moves fox up" $
        applyMove (Move Fox North) initBoard  `shouldBe`  (Board (Position 0 1) ( (Position 1 7)
                                                                               , (Position 3 7)
                                                                               , (Position 5 7)
                                                                               , (Position 7 7)
                                                                               ))

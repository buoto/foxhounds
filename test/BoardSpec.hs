module BoardSpec where

import Test.Hspec
import Board

spec = do
  describe "showBoardCell" $ do
    it "prints fox" $
      showBoardCell (Board (Position 0 0) []) (Position 0 0) `shouldBe` 'F'
    it "prints hound" $
      showBoardCell (Board (Position 0 0) [(Position 0 1)]) (Position 0 1) `shouldBe` 'H'
    it "prints empty" $
      showBoardCell (Board (Position 0 0) []) (Position 0 1) `shouldBe` '.'
  describe "showBoardRow" $ do
    it "prints 7th initBoard row" $
        showBoardRow initBoard 7 `shouldBe` ".H.H.H.H"
  describe "showBoard" $ do
    it "prints initBoard" $
        showBoard initBoard `shouldBe` ".H.H.H.H\n........\n........\n........\n........\n........\n........\nF.......\n"
  describe "concWithNewline" $ do
    it "adds \\n inside" $
        concWithNewline "a" "b" `shouldBe` "a\nb"

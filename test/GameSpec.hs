module GameSpec where

import Test.Hspec
import Game
import Board (initBoard)

spec = do
  describe "nextState" $ do
    it "gives hounds turn in initial" $
      nextState initGame `shouldBe` (Turn PlayerHounds)
    it "gives hounds turn in non winning state" $
      nextState (Game initBoard (Turn PlayerFox))  `shouldBe` (Turn PlayerHounds)
    it "gives fox turn in non winning state" $
      nextState (Game initBoard (Turn PlayerHounds)) `shouldBe` (Turn PlayerFox)
    it "stays in fox winning state" $
      nextState (Game initBoard (Winner PlayerFox)) `shouldBe` (Winner PlayerFox)
    it "stays in hounds winning state" $
      nextState (Game initBoard (Winner PlayerHounds)) `shouldBe` (Winner PlayerHounds)

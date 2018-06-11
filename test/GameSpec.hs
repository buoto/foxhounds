module GameSpec where

import Test.Hspec
import Game
import Board

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
    it "blocked fox looses" $
      nextState (Game (Board (Position 0 4) ( (Position 1 5), (Position 7 7), (Position 5 7), (Position 1 3)))
        (Turn PlayerFox)) `shouldBe` (Winner PlayerHounds)
    it "fox at the 7 row wins" $
      nextState (Game (Board (Position 0 7) ( (Position 1 5), (Position 3 7), (Position 5 7), (Position 7 7)))
        (Turn PlayerFox)) `shouldBe` (Winner PlayerFox)
    it "blocked hounds loose" $
      nextState (Game (Board (Position 1 3) ( (Position 0 4), (Position 2 0), (Position 4 0), (Position 6 0)))
        (Turn PlayerHounds)) `shouldBe` (Winner PlayerFox)

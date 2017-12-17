module HeuristicSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Heuristic" $ do
    it "should detect a red win in a column" $ do
      heuristic [[Just Red, Just Red], [Nothing, Nothing]] `shouldBe` 100

    it "should detect a blue win in a column" $ do
      heuristic [[Just Blue, Just Blue], [Nothing, Nothing]] `shouldBe` -100

    it "should calculate a zero game" $ do
      heuristic [[Nothing, Nothing], [Nothing, Nothing]] `shouldBe` 0

    it "should give equal weight to red and blue" $ do
      heuristic [[Just Red, Nothing], [Nothing, Just Blue]] `shouldBe` 0

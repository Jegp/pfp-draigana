module HeuristicSpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Sequence as Seq
import Dragon

main :: IO ()
main = hspec spec

boardFromList :: [[Field]] -> Board
boardFromList fields =
  Seq.fromList (fmap Seq.fromList fields)

spec :: Spec
spec = do
  describe "game heuristic" $ do
    it "should detect a red win in a column" $ do
      heuristic (boardFromList [[Just Red, Just Red], [Nothing, Nothing]]) `shouldBe` 100

    -- it "should detect a blue win in a column" $ do
    --   heuristic [[Just Blue, Just Blue], [Nothing, Nothing]]) `shouldBe` -100
    --
    -- it "should calculate a zero game" $ do
    --   heuristic [[Nothing, Nothing], [Nothing, Nothing]] `shouldBe` 0
    --
    -- it "should give equal weight to red and blue" $ do
    --   heuristic [[Just Red, Nothing], [Nothing, Just Blue]] `shouldBe` 0

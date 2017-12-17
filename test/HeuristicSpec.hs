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
    it "should detect a red win in a row" $ do
      heuristic (Red, boardFromList [[Just Red, Just Red], [Nothing, Nothing]], (L, 1)) `shouldBe` 100

    it "should detect a blue win in a row" $ do
      heuristic (Red, boardFromList [[Just Blue, Just Blue], [Nothing, Nothing]], (L, 1)) `shouldBe` -100

    it "should detect a blue win in a column" $ do
      heuristic (Red, boardFromList [[Nothing, Just Blue], [Nothing, Just Blue]], (L, 1)) `shouldBe` -100

    it "should compute value for non-win red" $ do
      heuristic (Red, boardFromList [[Just Red, Nothing], [Nothing, Nothing]], (L, 1)) `shouldBe` 50

    it "should compute value for non-win blue" $ do
      heuristic (Red, boardFromList [[Nothing, Nothing], [Nothing, Just Blue]], (L, 1)) `shouldBe` -50

    it "should calculate a zero game" $ do
      heuristic (Red, boardFromList [[Nothing, Nothing], [Nothing, Nothing]], (L, 1)) `shouldBe` 0

    it "should calculate a zero game with non-zero dragons" $ do
      heuristic (Red, boardFromList [[Nothing, Just Red], [Nothing, Just Blue]], (L, 1)) `shouldBe` 0

    it "should give equal weight to red and blue" $ do
      heuristic (Red, boardFromList [[Just Red, Nothing], [Nothing, Just Blue]], (L, 1)) `shouldBe` 0

    it "should calculate a negative red win" $ do
      heuristic (Blue, boardFromList [[Just Red, Nothing], [Just Red, Nothing]], (L, 1)) `shouldBe` -100

    it "should find a red win in a 3x3 game" $ do
      heuristic (Blue, boardFromList [[Nothing, Just Blue, Nothing]
                                     , [Just Red, Just Red, Just Red]
                                     , [Nothing, Just Blue, Just Blue]], (R, 2)) `shouldBe` -100

module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import qualified Data.Sequence as Seq

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pushing on a board" $ do
    it "should insert a piece in an empty list from the left" $ do
      insertDragonList Red (Seq.fromList [Nothing, Nothing]) `shouldBe`
        (Seq.fromList [Nothing, Just Red])

    it "should insert a piece in a half full list" $ do
      let board = Seq.fromList [Nothing, Just Red]
          expected = Seq.fromList [Just Red, Just Red]
      ; insertDragonList Red board `shouldBe` expected

    it "should insert a piece from the left with space left to the right" $ do
      let board = Seq.fromList [Nothing, Just Red, Nothing]
          expected = Seq.fromList [Just Red, Just Red, Nothing]
      ; insertDragonList Red board `shouldBe` expected

    it "should rotate a row entirely if no free space is available" $ do
      insertDragonList Red (Seq.fromList [Just Red, Just Blue]) `shouldBe`
        (Seq.fromList [Just Red, Just Red])

    it "should push the first elements in a row, but leave remaining dragons" $ do
      let board = Seq.fromList [Just Red, Nothing, Nothing]
          expected = Seq.fromList [Just Blue, Just Red, Nothing]
      ; insertDragonList Blue board `shouldBe` expected

  describe "inserting move" $ do
    let board = Seq.fromList [Seq.fromList [Nothing, Nothing], Seq.fromList [Nothing, Nothing]]
    it "should insert a move from the left" $ do
      insertDragon (Blue, board) (L, 1) `shouldBe` (Red,
        Seq.fromList [Seq.fromList [Nothing, Just Blue], Seq.fromList [Nothing, Nothing]])

    it "should insert a move from the right" $ do
      insertDragon (Red, board) (R, 2) `shouldBe` (Blue,
        Seq.fromList [Seq.fromList [Nothing, Nothing], Seq.fromList [Just Red, Nothing]])

    it "should insert a move from the top" $ do
      insertDragon (Blue, board) (T, 1) `shouldBe` (Red,
        Seq.fromList [Seq.fromList [Nothing, Nothing], Seq.fromList [Just Blue, Nothing]])

    it "should insert a move from the bottom" $ do
      insertDragon (Blue, board) (B, 2) `shouldBe` (Red,
        Seq.fromList [Seq.fromList [Nothing, Just Blue], Seq.fromList [Nothing, Nothing]])

    it "should insert many moves from an Incomplete" $ do
      boardFromIncomplete (2, [(L, 1), (B, 1), (B, 2)]) `shouldBe` (Blue,
        Seq.fromList [Seq.fromList [Just Blue, Just Red], Seq.fromList [Nothing, Just Red]])

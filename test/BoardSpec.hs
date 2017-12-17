module BoardSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import qualified Data.Sequence as Seq

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "A Board" $ do
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

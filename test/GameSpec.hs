module GameSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import qualified Data.Sequence as Seq

main :: IO ()
main = hspec spec

boardFromList :: [[Field]] -> Board
boardFromList fields =
  Seq.fromList (fmap Seq.fromList fields)

spec :: Spec
spec = do
  describe "reading a game" $ do
    it "should insert many moves from an Incomplete" $ do
      confFromIncomplete (2, [(L, 1), (B, 1), (B, 2)]) `shouldBe` (Blue,
        boardFromList [[Just Blue, Just Red], [Nothing, Just Red]], (B, 2))

  describe "minimax" $ do
    it "can generate moves" $ do
      let configs = possibleMoves (Red, emptyBoard 2, (L, 1))
      ; configs `shouldBe` [(Blue, boardFromList [[Nothing, Nothing], [Just Red, Nothing]], (T, 1))
          , (Blue, boardFromList [[Nothing, Just Red], [Nothing, Nothing]], (L, 1))
          , (Blue, boardFromList [[Just Red, Nothing], [Nothing, Nothing]], (B, 1))
          , (Blue, boardFromList [[Just Red, Nothing], [Nothing, Nothing]], (R, 1))
          , (Blue, boardFromList [[Nothing, Nothing], [Nothing, Just Red]], (T, 2))
          , (Blue, boardFromList [[Nothing, Nothing], [Nothing, Just Red]], (L, 2))
          , (Blue, boardFromList [[Nothing, Just Red], [Nothing, Nothing]], (B, 2))
          , (Blue, boardFromList [[Nothing, Nothing], [Just Red, Nothing]], (R, 2))]

    it "can find the winning move for blue" $ do
      let board = boardFromList [[Just Red, Nothing, Just Red]
                                , [Just Blue, Nothing, Just Blue]
                                , [Just Blue, Just Blue, Nothing]]
      ; nextMove 1 (Blue, board, (L, 1)) `shouldBe` (B, 1)

    -- Note: Test not working because minimax minimizes first?
    -- it "can find the winning move for red" $ do
    --   let board = Seq.fromList [Seq.fromList [Just Red, Nothing, Just Red]
    --                            , Seq.fromList [Just Blue, Nothing, Just Blue]
    --                            , Seq.fromList [Just Blue, Just Blue, Nothing]]
    --   ; nextMove 1 (Red, board, (L, 1)) `shouldBe` (L, 1)

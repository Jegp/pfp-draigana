module GameSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import qualified Data.Sequence as Seq

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "reading a game" $ do
    it "should insert many moves from an Incomplete" $ do
      confFromIncomplete (2, [(L, 1), (B, 1), (B, 2)]) `shouldBe` (Blue,
        Seq.fromList [Seq.fromList [Just Blue, Just Red], Seq.fromList [Nothing, Just Red]], (B, 2))

  describe "minimax" $ do
    it "can generate moves" $ do
      let configs = possibleMoves (Red, emptyBoard 2, (L, 1))
          moves = map (\(_, _, m) -> m) configs
      ; moves `shouldBe` [(T, 1), (L, 1), (B, 1), (R, 1),
        (T, 2), (L, 2), (B, 2), (R, 2)]

    it "can find the winning move for red" $ do
      let board = Seq.fromList [Seq.fromList [Just Red, Nothing, Just Red]
                               , Seq.fromList [Just Blue, Nothing, Just Blue]
                               , Seq.fromList [Just Blue, Just Blue, Just Blue]]
      ; heuristic (Red, board, (R, 3)) `shouldBe` -100 --nextMove 1 (Red, board, (L, 1)) `shouldBe` (L, 1)
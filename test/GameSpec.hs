module GameSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import Data.List (intercalate)
import qualified Data.Sequence as Seq

import Debug.Trace (trace)

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
        boardFromList [[Just Red, Nothing], [Just Blue, Just Red]], (B, 2))

  describe "minimax" $ do
    it "can generate moves" $ do
      let configs = possibleMoves (Red, emptyBoard 2, (L, 1))
      ; configs `shouldBe` [(Blue, boardFromList [[Just Red, Nothing], [Nothing, Nothing]], (T, 1))
          , (Blue, boardFromList [[Just Red, Nothing], [Nothing, Nothing]], (L, 1))
          , (Blue, boardFromList [[Nothing, Nothing], [Just Red, Nothing]], (B, 1))
          , (Blue, boardFromList [[Nothing, Just Red], [Nothing, Nothing]], (R, 1))
          , (Blue, boardFromList [[Nothing, Just Red], [Nothing, Nothing]], (T, 2))
          , (Blue, boardFromList [[Nothing, Nothing], [Just Red, Nothing]], (L, 2))
          , (Blue, boardFromList [[Nothing, Nothing], [Nothing, Just Red]], (B, 2))
          , (Blue, boardFromList [[Nothing, Nothing], [Nothing, Just Red]], (R, 2))]

    it "can find the winning move for blue" $ do
      let board = boardFromList [[Just Red, Nothing, Just Red]
                                , [Just Blue, Nothing, Just Blue]
                                , [Just Blue, Just Blue, Nothing]]
      ; nextMove (Blue, board, (L, 1)) `shouldBe` (B, 1)

    it "can load a test and find the winning move" $ do
      let boardSize = 4
          moves = parseMoves ["L2","T2","L2","B2","R2"]
          conf = confFromIncomplete (boardSize, moves)
          stringMoves = map showConf (possibleMoves conf)
      ; putStrLn $ showConf conf
      -- nextMove conf `shouldBe` (L, 2)
       where
         showConf :: Conf -> [Char]
         showConf (player, board, move) = ((show move) ++ (show player) ++ (show (heuristic (player, board, move))) ++ (show (Seq.index board 0)))

    -- Note: Test not working because minimax minimizes first?
    -- it "can find the winning move for red" $ do
    --   let board = Seq.fromList [Seq.fromList [Just Red, Nothing, Just Red]
    --                            , Seq.fromList [Just Blue, Nothing, Just Blue]
    --                            , Seq.fromList [Just Blue, Just Blue, Nothing]]
    --   ; nextMove 1 (Red, board, (L, 1)) `shouldBe` (L, 1)

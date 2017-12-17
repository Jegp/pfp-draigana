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
        Seq.fromList [Seq.fromList [Just Blue, Just Red], Seq.fromList [Nothing, Just Red]])

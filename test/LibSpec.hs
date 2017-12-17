module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Dragon
import qualified Data.Sequence as Seq
import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Sequences transposition" $ do
    it "should transpose a simple sequence" $ do
      let matrix = Seq.fromList [Seq.fromList [1, 2], Seq.fromList [3, 4]]
      ; transpose matrix `shouldBe` Seq.fromList [Seq.fromList [1, 3], Seq.fromList [2, 4]]

    it "should transpose an empty sequence" $ do
      let matrix = (Seq.fromList []::(Seq.Seq (Seq.Seq Int)))
      ; transpose matrix `shouldBe` matrix

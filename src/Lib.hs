{-# LANGUAGE ViewPatterns #-}

module Lib
    ( transpose
    ) where

import Data.Sequence (Seq, ViewL)
import qualified Data.Sequence as Seq

-- Seq implementation of transpose, borrowed from
-- http://hackage.haskell.org/package/base-4.10.1.0/docs/src/Data.OldList.html#transpose
transpose :: Seq (Seq a) -> Seq (Seq a)
transpose matrix = case Seq.viewl matrix of
  Seq.EmptyL -> Seq.empty
  (first Seq.:< xss) ->
    if Seq.length first == 0 then transpose xss
    else let
      x = Seq.index first 0
      xs = Seq.drop 1 first
      listOfHeads = x Seq.<| (fmap (\s -> Seq.index s 0) xss)
      listOfTails = transpose (xs Seq.<| (fmap (\s -> Seq.drop 1 s) xss))
    in listOfHeads Seq.<| listOfTails

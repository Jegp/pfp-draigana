module Main where

import Dragon

main :: IO ()
main = do
  incomplete <- readIncomplete
  let m = nextMove (confFromIncomplete incomplete)
  printMove m

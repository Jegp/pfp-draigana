module Main where

import Dragon

main :: IO ()
main = do
  incomplete <- readIncomplete
  let m = nextMove 2 (confFromIncomplete incomplete)
  printMove m

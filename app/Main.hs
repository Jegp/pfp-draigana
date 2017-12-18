module Main where

import Dragon

main :: IO ()
main = do
  incomplete <- readIncomplete
  let timer = (10 * 1000 * 1000) - 1000 -- 10 seconds in microseconds (minus some)
  m <- nextMove timer (confFromIncomplete incomplete)
  printMove m

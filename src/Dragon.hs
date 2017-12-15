module Dragon where

import Data.List (intersperse,intercalate)


data Side = L | R | T | B
  deriving (Eq, Show, Read)

type Move = (Side, Int)

data Player = Red | Blue
  deriving (Eq, Show, Read)

type Incomplete = (Int      -- size of the board
                  , [Move]  -- moves made so far]
                  )

readIncomplete :: IO Incomplete
readIncomplete = do
  inp <- getContents
  let ls     = lines inp
      n      = read $ head ls
      moves  = parseMoves $ tail ls
  return (n, moves)

parseMoves :: [String] -> [Move]
parseMoves ls = map parseMove ls
  where parseMove (s : idx) = (read [s], read idx)
        parseMove _ = error "Illegal move syntax"

printMove :: Move -> IO()
printMove (s, idx) =
  putStrLn $ show s ++ show idx


-- Dummy implementation, just repeats the last move or starts with T2
nextMove :: Incomplete -> Move
nextMove (_, []) = (T, 2)
nextMove (_, moves) = last moves


-- Representing boards with lists

type Board = [[Maybe Player]]
type Conf  = (Player, Board)

emptyBoard n = replicate n $ replicate n Nothing

showBoard :: Int -> Board -> String
showBoard n board = border ++ inner ++ border
  where
    border = "  " ++ (concat $ replicate n "+-") ++ "+\n"
    inner = intercalate border (map showLine board)
    showLine row = "  |" ++ (row >>= showPlayer) ++ "\n"
    showPlayer (Just Red) = "R|"
    showPlayer (Just _)   = "B|"
    showPlayer _          = " |"

toggle Red  = Blue
toggle Blue = Red

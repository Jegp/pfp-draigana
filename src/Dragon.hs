module Dragon where

import Data.List (intersperse,intercalate,transpose)


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

--
-- Heuristics for board configurations
--

-- A 'Streak' is the longest coherent number of pieces
type Streak = Int

-- Defines the game value for players Red and Blue
data GameValue = GameValue { red :: Streak, blue :: Streak }

maxGameValue :: GameValue -> GameValue -> GameValue
maxGameValue (GameValue r1 b1) (GameValue r2 b2) =
  (GameValue (if r1 > r2 then r1 else r2) (if b1 > b2 then b1 else b2))

sumGameValue :: GameValue -> GameValue -> GameValue
sumGameValue (GameValue r1 b1) (GameValue r2 b2) = (GameValue (r1 + r2) (b1 + b2))

collectValue :: GameValue -> Maybe Player -> GameValue
collectValue v Nothing = v
collectValue (GameValue r b) (Just Red) = (GameValue (r + 1) b)
collectValue (GameValue r b) (Just Blue) = (GameValue r (b + 1))

boardToGameValue :: Board -> GameValue
boardToGameValue rows =
  let startValue = (GameValue 0 0)
      rowValue = map (\row -> foldl collectValue startValue row) rows
      columnValue = map (\column -> foldl collectValue startValue column) (transpose rows)
      rowSum = foldl maxGameValue startValue rowValue
      columnSum = foldl maxGameValue startValue columnValue
  in maxGameValue rowSum columnSum

-- Calculates the heuristic for a game where +100 is a red win and -100 a blue win
heuristic :: Board -> Float
heuristic board =
  let (GameValue red blue) = boardToGameValue board
  in (normalize red) - (normalize blue) where
    normalize n = (fromIntegral (100 * n)) / (fromIntegral (length board))

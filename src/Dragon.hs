module Dragon where

import Data.Maybe (isJust, isNothing)
import Lib
import Minmax (aimove)

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Debug.Trace (trace)

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

-- Finds the next move to a given depth
nextMove :: Int -> Conf -> Move
nextMove depth (player, board, move) =
  let (_, _, bestMove) = aimove depth possibleMoves heuristic (player, board, move)
  in bestMove

-- Generates possible moves from a board of size n
possibleMoves :: Conf -> [Conf]
possibleMoves (player, board, move) =
  map execute $ concatMap (\i -> [(T, i), (L, i), (B, i), (R, i)]) [1..(Seq.length board)]
  where
    execute :: Move -> Conf
    execute newMove = insertDragon (player, board, newMove)

-- Representing boards with sequences
type Field = Maybe Player
type Board = Seq (Seq Field)
type Conf  = (Player, Board, Move)

emptyBoard :: Int -> Board
emptyBoard n = Seq.replicate n $ Seq.replicate n Nothing

toggle Red  = Blue
toggle Blue = Red

confFromIncomplete :: Incomplete -> Conf
confFromIncomplete (n, moves) =
  let board = emptyBoard n
  in foldl (\(p, b, _) newMove -> insertDragon (p, b, newMove)) (Red, board, (L, 1)) moves

insertDragon :: Conf -> Conf
insertDragon (player, board, (side, i)) =
  let index = i - 1 -- moves are 1-indexed
      newPlayer = toggle player
  in case side of
    L -> let updatedRow = insertDragonList player (Seq.index board index)
         in (newPlayer, Seq.update index updatedRow board, (side, i))
    R -> let reversedRow = Seq.reverse (Seq.index board index)
             updatedRow = insertDragonList player reversedRow
         in (newPlayer, Seq.update index (Seq.reverse updatedRow) board, (side, i))
    T -> let transposed = transpose board
             updatedRow = insertDragonList player (Seq.index transposed index)
         in (newPlayer, transpose (Seq.update index updatedRow transposed), (side, i))
    B -> let transposed = transpose board
             reversedRow = Seq.reverse (Seq.index transposed index)
             updatedRow = insertDragonList player reversedRow
         in (newPlayer, transpose (Seq.update index (Seq.reverse updatedRow) transposed), (side, i))

insertDragonList :: Player -> Seq Field -> Seq Field
insertDragonList player list =
  case Seq.findIndexL isJust list of
    Just 0 ->
      let appendedList = (Just player) Seq.<| list
      in case Seq.elemIndexL Nothing appendedList of
        Nothing -> Seq.take (Seq.length list) appendedList
        Just index -> Seq.deleteAt index appendedList
    Just n -> Seq.update (n - 1) (Just player) list
    Nothing -> Seq.drop 1 (list Seq.|> (Just player))

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
      rowValue = fmap (\row -> foldl collectValue startValue row) rows
      columnValue = fmap (\column -> foldl collectValue startValue column) (transpose rows)
      rowSum = foldl maxGameValue startValue rowValue
      columnSum = foldl maxGameValue startValue columnValue
  in maxGameValue rowSum columnSum

-- Calculates the heuristic for a game where +100 is a red win and -100 a blue win
heuristic :: Conf -> Int
heuristic (player, board, _) =
  let (GameValue red blue) = boardToGameValue board
      scale = if player == Red then 1 else -1
  in case (normalize red, normalize blue) of
    (100, _) -> 100 * scale
    (_, 100) -> -100 * scale
    (red, blue) -> (red - blue) * scale
  where normalize n = (100 * n) `quot` (length board)

module Dragon where

import Data.Maybe (isJust, isNothing)
import Lib
import Minmax (aimove)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.QSemN

import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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

showBoard :: Int -> Board -> String
showBoard n seqBoard = border ++ inner ++ border
  where
    board :: [[Field]]
    board = toList (fmap toList board)
    border = "  " ++ (concat $ replicate n "+-") ++ "+\n"
    inner = intercalate border (map showLine board)
    showLine row = "  |" ++ (row >>= showPlayer) ++ "\n"
    showPlayer (Just Red) = "R|"
    showPlayer (Just _)   = "B|"
showPlayer _ = " |"

-- Finds the next move within a given time in microseconds
nextMove :: Int -> Conf -> IO Move
nextMove timer conf = do
  bestVar <- newEmptyMVar
  forkIO $ asyncNextMove bestVar 1 conf
  threadDelay timer
  ((_, _, bestMove), _) <- readMVar bestVar
  return bestMove

-- aimove depth moves static p
asyncNextMove :: MVar (Conf, Int) -> Int -> Conf -> IO ()
asyncNextMove var depth conf = do
    let moves = possibleMoves conf
    children <- newQSemN 0
    ids <- mapM (\c -> forkIO (synchNextMove children var (depth + 1) c)) moves
    waitQSemN children (length moves)
    asyncNextMove var (depth + 1) conf

synchNextMove :: QSemN -> MVar (Conf, Int) -> Int -> Conf -> IO ()
synchNextMove semaphore var depth conf = do
  let bestConf = aimove depth possibleMoves heuristic conf
  currentVar <- tryTakeMVar var
  case currentVar of
    Nothing -> putMVar var (bestConf, depth)
    Just (currentConf, currentDepth) ->
      let h1 = heuristic currentConf
          h2 = heuristic bestConf
      in if h1 >= 100 || h1 <= -100 then return ()
      else if currentDepth < depth then return ()
      else if (heuristic currentConf) > (heuristic bestConf) then return ()
      else putMVar var (bestConf, depth)
  signalQSemN semaphore 1

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
  case Seq.index list 0 of
    Nothing -> Seq.update 0 (Just player) list
    Just _ ->
      let appendedList = (Just player) Seq.<| list
      in case Seq.elemIndexL Nothing appendedList of
        Nothing -> Seq.take (Seq.length list) appendedList
        Just index -> Seq.deleteAt index appendedList

--
-- Heuristics for board configurations
--

-- A 'Line of power' is a full line of one colour
type LOP = Int

-- A 'Streak' is the longest coherent number of pieces
type Streak = Int

-- Defines the game value for players Red and Blue
data GameValue a = GameValue { red :: a, blue :: a }
  deriving (Show, Eq)

maxGameValue :: GameValue Streak -> GameValue Streak -> GameValue Streak
maxGameValue (GameValue r1 b1) (GameValue r2 b2) =
  (GameValue (if r1 > r2 then r1 else r2) (if b1 > b2 then b1 else b2))

sumGameValue :: GameValue Streak -> GameValue Streak -> GameValue Streak
sumGameValue (GameValue r1 b1) (GameValue r2 b2) = (GameValue (r1 + r2) (b1 + b2))

collectValue :: GameValue Streak -> Maybe Player -> GameValue Streak
collectValue v Nothing = v
collectValue (GameValue r b) (Just Red) = (GameValue (r + 1) b)
collectValue (GameValue r b) (Just Blue) = (GameValue r (b + 1))

collectLop :: Int -> GameValue LOP -> GameValue Streak -> GameValue LOP
collectLop size (GameValue lr lb) (GameValue red blue) =
  let blueLop = if blue == size then 1 else 0
      redLop = if red == size then 1 else 0
  in (GameValue (redLop + lr) (blueLop + lb))

boardToGameValue :: Board -> (GameValue Streak, GameValue LOP)
boardToGameValue rows =
  let startValue = (GameValue 0 0)
      rowValue = fmap (\row -> foldl collectValue startValue row) rows
      columnValue = fmap (\column -> foldl collectValue startValue column) (transpose rows)
      rowSum = foldl sumGameValue startValue rowValue
      columnSum = foldl sumGameValue startValue columnValue
      boardSize = Seq.length rows
      rop = foldl (collectLop boardSize) startValue rowValue
      cop = foldl (collectLop boardSize) startValue columnValue
  in (maxGameValue rowSum columnSum, sumGameValue rop cop)

-- Calculates the heuristic for a game where +100 is a red win and -100 a blue win
heuristic :: Conf -> Int
heuristic (player, board, _) =
  let (GameValue red blue, GameValue redLop blueLop) = boardToGameValue board
      scale = if player == Red then 1 else -1
      value = if redLop > blueLop then (redLop * 100)
        else if blueLop < redLop then (blueLop * 100)
        else if redLop /= 0 then 100 -- playing player lost
        else ((normalize red) - (normalize blue))
  in value * scale -- Scale the value based on the current player playing
  where
    normalize n = (100 * n) `quot` (length board)

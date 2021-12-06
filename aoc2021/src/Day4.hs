{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Parser (Parser)
import Text.Megaparsec (many, sepBy, sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data GridNum = One | Two | Three | Four | Five deriving (Eq, Show, Ord, Bounded, Enum)

newtype ColNum = ColNum GridNum deriving (Eq, Show, Ord, Bounded, Enum)

newtype RowNum = RowNum GridNum deriving (Eq, Show, Ord, Bounded, Enum)

data BingoBoard = BingoBoard
  { board :: M.Map Int (ColNum, RowNum),
    checkSquare :: ColNum -> RowNum -> Bool,
    hasWon :: Bool
  }

instance Show BingoBoard where
  show (BingoBoard {board}) = show board

data Puzzle = Puzzle
  { boards :: [BingoBoard],
    callSequence :: [Int]
  }

instance Show Puzzle where
  show (Puzzle {boards, callSequence}) = "Puzzle " ++ show boards ++ " " ++ show callSequence

data GameState = GameState
  { puzzle :: Puzzle,
    winner :: Maybe (BingoBoard, Int)
  }

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 xs = [xs]
chunksOf n xs = [take n xs] ++ chunksOf n (drop n xs)

debugBoard :: BingoBoard -> String
debugBoard BingoBoard {board, checkSquare} =
  let sorted = sortBy (\(_, coord) (_, coord') -> compare (swap coord) (swap coord')) (M.toList board)
      withXes = (\(k, (c, r)) -> if checkSquare c r then "x" else show k) <$> sorted
   in unlines $ unwords <$> chunksOf 5 withXes

mark :: Int -> BingoBoard -> BingoBoard
mark value bingoBoard = case M.lookup value (board bingoBoard) of
  Nothing -> bingoBoard
  Just (col, row) ->
    let baseCheckSquare = checkSquare bingoBoard
     in bingoBoard
          { checkSquare = \col' row' -> (col' == col && row' == row) || baseCheckSquare col' row'
          }

markWinner :: Int -> BingoBoard -> BingoBoard
markWinner call bingoBoard =
  case M.lookup call (board bingoBoard) of
    Just (col, row) ->
      let marked = mark call bingoBoard
       in marked
            { hasWon = checkWinner col row marked
            }
    Nothing -> bingoBoard

checkWinner :: ColNum -> RowNum -> BingoBoard -> Bool
checkWinner c r b = checkColumn c b || checkRow r b

checkColumn :: ColNum -> BingoBoard -> Bool
checkColumn col (BingoBoard {checkSquare}) =
  all (\row -> checkSquare col row) [minBound .. maxBound]

checkRow :: RowNum -> BingoBoard -> Bool
checkRow row (BingoBoard {checkSquare}) =
  all (\col -> checkSquare col row) [minBound .. maxBound]

nbsp :: Parser [Char]
nbsp = many (char ' ')

mapBoards :: Puzzle -> (BingoBoard -> BingoBoard) -> Puzzle
mapBoards puzz@(Puzzle {boards}) f = puzz {boards = f <$> boards}

filterBoards :: Puzzle -> (BingoBoard -> Bool) -> Puzzle
filterBoards puzz f = puzz {boards = filter f . boards $ puzz}

rowParser :: Parser [(ColNum, Int)]
rowParser = nbsp *> decimal `sepBy` nbsp <&> zip [minBound .. maxBound]

boardParser :: Parser BingoBoard
boardParser =
  let colsFold rowIdx acc = foldl' (\acc' (colIdx, v) -> M.insert v (colIdx, rowIdx) acc') acc
   in replicateM 5 (rowParser <* eol) <&> zip [minBound .. maxBound]
        <&> foldl' (\acc (rowIdx, cols) -> colsFold rowIdx acc cols) M.empty
        <&> \m -> BingoBoard m (\_ _ -> False) False

callSequenceParser :: Parser [Int]
callSequenceParser = decimal `sepBy` char ','

puzzleParser :: Parser Puzzle
puzzleParser = do
  callSequence <- callSequenceParser
  _ <- eol *> eol
  boards <- boardParser `sepEndBy` eol
  pure $ Puzzle boards callSequence

findWinner :: Int -> Puzzle -> Maybe BingoBoard
findWinner n (Puzzle {boards}) =
  foldl'
    ( \acc bingo@(BingoBoard {board}) ->
        case (\(c, r) -> checkWinner c r bingo) <$> M.lookup n board of
          Nothing -> acc
          Just False -> acc
          Just True -> Just bingo
    )
    Nothing
    boards

callNumber :: Int -> GameState -> GameState
callNumber n (GameState {puzzle}) =
  let marked = mapBoards puzzle (mark n)
      winner = findWinner n marked
      winningCall = if null winner then Nothing else Just n
   in GameState marked (liftA2 (,) winner winningCall)

findUnmarkedScore :: BingoBoard -> Int
findUnmarkedScore BingoBoard {board, checkSquare} =
  let keys = M.toList board
   in foldl'
        ( \acc (k, (c, r)) ->
            if checkSquare c r then acc else acc + k
        )
        0
        keys

evaluateGame1 :: Puzzle -> GameState
evaluateGame1 puzz@(Puzzle {callSequence}) =
  foldl'
    ( \gameState@(GameState {winner}) call ->
        case winner of
          (Just _) -> gameState
          Nothing -> callNumber call gameState
    )
    (GameState puzz Nothing)
    callSequence

evaluateGame2 :: Puzzle -> GameState
evaluateGame2 puzz@(Puzzle {callSequence}) =
  foldl'
    ( \gameState@(GameState {puzzle}) call ->
        case boards puzzle of
          [] -> gameState
          [x] ->
            let marked = markWinner call x
             in if (hasWon marked)
                  then
                    gameState
                      { puzzle = filterBoards puzzle (const False),
                        winner = Just (marked, call)
                      }
                  else gameState {puzzle = Puzzle [marked] []}
          _ ->
            let marked = mapBoards puzzle (markWinner call)
             in gameState {puzzle = (filterBoards marked (not . hasWon))}
    )
    (GameState puzz Nothing)
    callSequence

solvePart1 :: Puzzle -> Int
solvePart1 puzz =
  case evaluateGame1 puzz of
    (GameState _ (Just (b, c))) ->
      findUnmarkedScore b * c
    _ -> 0

solvePart2 :: Puzzle -> Int
solvePart2 puzz =
  case evaluateGame2 puzz of
    GameState _ (Just (b, c)) ->
      findUnmarkedScore b * c
    _ -> 1

solve :: Puzzle -> Int
solve = solvePart2

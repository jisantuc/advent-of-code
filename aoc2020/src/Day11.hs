module Day11 where

import Control.Applicative (Applicative (liftA2))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, eol)

data Seat = Empty | Occupied | Floor deriving (Eq, Show)

type FloorChart =
  M.Map
    (Int, Int)
    Seat

minX :: M.Map (Int, Int) a -> Int
minX c = minimum $ fst <$> M.keys c

maxX :: M.Map (Int, Int) a -> Int
maxX c = maximum $ fst <$> M.keys c

minY :: M.Map (Int, Int) a -> Int
minY c = minimum $ snd <$> M.keys c

maxY :: M.Map (Int, Int) a -> Int
maxY c = maximum $ snd <$> M.keys c

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

grid :: FloorChart -> [(Int, Int)]
grid chart =
  let xs = [minX chart .. maxX chart]
      ys = [minY chart .. maxY chart]
   in liftA2 (,) xs ys

stepSeat :: FloorChart -> (Int, Int) -> ((Int, Int), Seat)
stepSeat chart coord =
  let adjacentCoords = neighbors coord
      thisSeat = getSeat chart coord
      occupiedNeighbors =
        foldl'
          ( \acc c ->
              case getSeat chart c of
                Empty -> acc
                Floor -> acc
                Occupied -> acc + 1
          )
          0
          adjacentCoords
   in case (thisSeat, occupiedNeighbors) of
        (Floor, _) -> (coord, Floor)
        (Empty, 0) -> (coord, Occupied)
        (Empty, _) -> (coord, Empty)
        (Occupied, n) ->
          if (n >= 4) then (coord, Empty) else (coord, Occupied)

getSeat :: FloorChart -> (Int, Int) -> Seat
getSeat chart coord = fromMaybe Empty (M.lookup coord chart)

step :: FloorChart -> FloorChart
step chart =
  let locations = grid chart
   in M.fromList $ stepSeat chart <$> locations

seatParser :: Parser Seat
seatParser =
  Empty <$ char 'L'
    <|> Occupied <$ char '#'
    <|> Floor <$ char '.'

indexList :: Show a => [[a]] -> [((Int, Int), a)]
indexList as = do
  (y, row) <- zip [1 ..] as
  (x, a) <- zip [1 ..] row
  pure $ ((x, y), a)

puzzleParser :: Parser FloorChart
puzzleParser = do
  seats <- sepEndBy (some seatParser) eol
  let indexed = indexList seats
  pure $ M.fromList indexed

countOccupied :: FloorChart -> Int
countOccupied =
  M.foldl'
    ( \acc seat -> case seat of
        Occupied -> acc + 1
        _ -> acc
    )
    0

solve :: FloorChart -> Int
solve chart =
  let newChart = step chart
   in if (newChart == chart) then countOccupied newChart else solve newChart

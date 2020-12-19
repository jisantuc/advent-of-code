module Day11 where

import Control.Applicative (Applicative (liftA2))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, some, (<|>))
import Text.Megaparsec.Char (char, eol)

data Seat = Empty | Occupied | Floor deriving (Eq, Show)

type Coord = (Int, Int)

type FloorChart =
  M.Map
    Coord
    Seat

signedStep :: Int -> Int
signedStep n
  | n == 0 = 0
  | abs n == n = 1
  | otherwise = -1

nextStep :: Coord -> Coord -> Coord
nextStep (neighborX, neighborY) (hostX, hostY) =
  let dX = signedStep (neighborX - hostX)
      dY = signedStep (neighborY - hostY)
   in (neighborX + dX, neighborY + dY)

minX :: M.Map Coord a -> Int
minX c = minimum $ fst <$> M.keys c

maxX :: M.Map Coord a -> Int
maxX c = maximum $ fst <$> M.keys c

minY :: M.Map Coord a -> Int
minY c = minimum $ snd <$> M.keys c

maxY :: M.Map Coord a -> Int
maxY c = maximum $ snd <$> M.keys c

neighbors :: Coord -> [Coord]
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

grid :: FloorChart -> [Coord]
grid chart =
  let xs = [minX chart .. maxX chart]
      ys = [minY chart .. maxY chart]
   in liftA2 (,) xs ys

checkNeighbor :: Coord -> Coord -> FloorChart -> Seat
checkNeighbor neighbor host chart =
  case getSeat chart neighbor of
    Floor -> checkNeighbor (nextStep neighbor host) host chart
    seat -> seat

stepSeat :: FloorChart -> Coord -> (Coord, Seat)
stepSeat chart coord =
  let adjacentCoords = neighbors coord
      thisSeat = getSeat chart coord
      occupiedNeighbors =
        foldl'
          ( \acc c ->
              case checkNeighbor c coord chart of
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
          if (n >= 5) then (coord, Empty) else (coord, Occupied)

getSeat :: FloorChart -> Coord -> Seat
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

indexList :: Show a => [[a]] -> [(Coord, a)]
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
  countWhen (== Occupied)

countWhen :: (Seat -> Bool) -> FloorChart -> Int
countWhen p = M.foldl' (\acc seat -> if (p seat) then acc + 1 else acc) 0

solve :: FloorChart -> Int
solve chart =
  let newChart = step chart
   in if (newChart == chart) then countOccupied newChart else solve newChart

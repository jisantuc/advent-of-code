{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (..))
import Debug (chunksOf)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, skipSome)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

newtype XCoord = XCoord Int deriving (Enum, Eq, Num, Ord, Show)

newtype YCoord = YCoord Int deriving (Enum, Eq, Num, Ord, Show)

data Location = Location XCoord YCoord deriving (Eq, Ord, Show)

data Segment = Segment {start :: Location, end :: Location} deriving (Eq, Show)

type Puzzle = [Segment]

location :: Int -> Int -> Location
location x y = Location (XCoord x) (YCoord y)

segmentParser :: Parser Segment
segmentParser =
  do
    x1 <- XCoord <$> decimal
    skipSome $ char ','
    y1 <- YCoord <$> decimal
    skipSome $ string " -> "
    x2 <- XCoord <$> decimal
    skipSome $ char ','
    y2 <- YCoord <$> decimal
    pure $ Segment (Location x1 y1) (Location x2 y2)

bbox :: Puzzle -> (XCoord, YCoord, XCoord, YCoord)
bbox [] = (XCoord 0, YCoord 0, XCoord 0, YCoord 0)
bbox segments =
  foldl'
    ( \(minX, minY, maxX, maxY) (Segment (Location startX startY) (Location endX endY)) ->
        ( minimum [minX, startX, endX],
          minimum [minY, startY, endY],
          maximum [maxX, startX, endX],
          maximum [maxY, startY, endY]
        )
    )
    (XCoord 0, YCoord 0, XCoord 0, YCoord 0)
    segments

isDiagonal :: Segment -> Bool
isDiagonal (Segment (Location xStart yStart) (Location xEnd yEnd)) =
  xStart /= xEnd && yStart /= yEnd

is45Degrees :: Segment -> Bool
is45Degrees
  ( Segment
      (Location (XCoord xStart) (YCoord yStart))
      (Location (XCoord xEnd) (YCoord yEnd))
    ) =
    xStart /= xEnd && yStart /= yEnd && abs (xEnd - xStart) == abs (yEnd - yStart)

puzzleParser :: Parser Puzzle
puzzleParser = segmentParser `sepEndBy` eol

grid :: Puzzle -> [Location]
grid [] = []
grid puzz =
  let (xMin, yMin, xMax, yMax) = bbox puzz
   in [Location x y | y <- [yMin .. yMax], x <- [xMin .. xMax]]

mkRange :: (Ord a, Enum a) => a -> a -> [a]
mkRange a b = case compare a b of
  EQ -> repeat a
  LT -> [a .. b]
  GT -> [b .. a]

fillPoints :: Segment -> [(Location, Sum Int)]
fillPoints seg@(Segment startLoc@(Location xStart yStart) endLoc@(Location xEnd yEnd)) =
  let pair as = (\x -> (x, Sum 1)) <$> as
   in case (isDiagonal seg, is45Degrees seg) of
        (False, _) ->
          let xes = mkRange xStart xEnd
              yes = mkRange yStart yEnd
           in pair $ zipWith Location xes yes
        (True, False) -> []
        (True, True) ->
          let xStep = signum (xEnd - xStart)
              yStep = signum (yEnd - yStart)
              stepper begin@(Location x y) stop deltaX deltaY =
                if begin == stop
                  then [begin]
                  else begin : stepper (Location (x + deltaX) (y + deltaY)) stop deltaX deltaY
           in pair $ stepper startLoc endLoc xStep yStep

fillPointsP :: (Segment -> Bool) -> Segment -> [(Location, Sum Int)]
fillPointsP predicate seg = if (predicate seg) then fillPoints seg else []

solveP :: (Segment -> Bool) -> Puzzle -> M.Map Location (Sum Int)
solveP f =
  foldl'
    ( \acc seg ->
        let locations = M.fromList $ fillPointsP f seg
         in M.unionWith (<>) acc locations
    )
    M.empty

solvePart1Fast :: Puzzle -> Int
solvePart1Fast puzz =
  length . M.filter (\x -> x >= 2) $
    solveP (not . isDiagonal) puzz

solvePart2 :: Puzzle -> Int
solvePart2 puzz =
  length
    . M.filter (\x -> x >= 2)
    $ solveP (\x -> (not . isDiagonal) x || is45Degrees x) puzz

solve :: Puzzle -> Int
solve = solvePart2

debugGrid :: (Puzzle -> M.Map Location (Sum Int)) -> Puzzle -> String
debugGrid solver puzz =
  let (_, _, XCoord xMax, _) = bbox puzz
      g = grid puzz
      covering = solver puzz
      printed =
        ( \case
            Nothing -> "."
            Just n -> show . getSum $ n
        )
          . (\loc -> M.lookup loc covering)
          <$> g
      chunked = chunksOf (xMax + 1) printed
   in unlines $ unwords <$> chunked

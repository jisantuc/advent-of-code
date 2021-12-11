{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Data.Foldable (Foldable (foldl'))
import Debug (chunksOf)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, skipSome)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

newtype XCoord = XCoord Int deriving (Enum, Eq, Num, Ord, Show)

newtype YCoord = YCoord Int deriving (Enum, Eq, Num, Ord, Show)

data Location = Location XCoord YCoord deriving (Eq, Show)

data Segment = Segment {start :: Location, end :: Location} deriving (Eq, Show)

type Puzzle = [Segment]

reachableFrom :: Location -> Segment -> Bool
reachableFrom
  (Location testX testY)
  (Segment (Location startX startY) (Location endX endY)) =
    case (compare startY endY, compare startX endX) of
      (EQ, EQ) -> testX == startX && testY == startY
      (LT, LT) -> False
      (LT, EQ) -> testX == startX && startY <= testY && testY <= endY
      (GT, EQ) -> testX == startX && startY >= testY && testY >= endY
      (EQ, LT) -> testY == startY && startX <= testX && testX <= endX
      (EQ, GT) -> testY == startY && startX >= testX && testX >= endX
      -- this fall through covers diagonals, which are filtered out in the parse
      _ -> False

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

puzzleParser :: Parser Puzzle
puzzleParser = filter (not . isDiagonal) <$> segmentParser `sepEndBy` eol

grid :: Puzzle -> [Location]
grid [] = []
grid puzz =
  let (xMin, yMin, xMax, yMax) = bbox puzz
   in [Location x y | y <- [yMin .. yMax], x <- [xMin .. xMax]]

solvePart1 :: Puzzle -> Int
solvePart1 [] = 0
solvePart1 puzz =
  let (xMin, yMin, xMax, yMax) = bbox puzz
      g = [Location x y | y <- [yMin .. yMax], x <- [xMin .. xMax]]
   in foldl'
        ( \acc loc ->
            let coveringSegments = filter (reachableFrom loc) puzz
             in if length coveringSegments >= 2
                  then acc + 1
                  else acc
        )
        0
        g

debugGrid :: Puzzle -> String
debugGrid puzz =
  let (_, _, XCoord xMax, _) = bbox puzz
      g = grid puzz
      covering = (\loc -> length $ filter (reachableFrom loc) puzz) <$> g
      printed =
        ( \case
            0 -> "."
            n -> show n
        )
          <$> covering
      chunked = chunksOf (xMax + 1) printed
   in unlines $ unwords <$> chunked

solve :: Puzzle -> Int
solve = solvePart1

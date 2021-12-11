{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (..))
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
fillPoints (Segment (Location xStart yStart) (Location xEnd yEnd)) =
  let xes = mkRange xStart xEnd
      yes = mkRange yStart yEnd
   in (\x -> (x, Sum 1)) <$> zipWith Location xes yes

solvePart1Fast :: Puzzle -> Int
solvePart1Fast puzz =
  let intersections =
        foldl'
          ( \acc seg ->
              if (isDiagonal seg)
                then acc
                else
                  let locations = M.fromList $ fillPoints seg
                   in M.unionWith (<>) acc locations
          )
          M.empty
          puzz
   in length . M.filter (\x -> x >= 2) $ intersections

solve :: Puzzle -> Int
solve = solvePart1Fast

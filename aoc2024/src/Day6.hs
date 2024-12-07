{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import AoC.Data.Grid.Parser (sourcePositionToPoint)
import AoC.Data.Grid.Rectangular (Direction (..), RectangularGrid (..))
import qualified AoC.Data.Grid.Rectangular as RectangularGrid
import AoC.Parser (Parser, fromMap)
import qualified Data.Map as Map
import Text.Megaparsec (getSourcePos, many, sepBy)
import Text.Megaparsec.Char (eol)

data FloorSpace = Open | Obstacle | Guard Direction deriving (Eq, Show)

spaceParser :: Parser ((Int, Int), FloorSpace)
spaceParser = do
  pos <- sourcePositionToPoint <$> getSourcePos
  space <- fromMap $ Map.fromList [(".", Open), ("#", Obstacle), ("^", Guard (Up 1))]
  pure (pos, space)

puzzleParser :: Parser (RectangularGrid ((Int, Int), FloorSpace))
puzzleParser =
  RectangularGrid.fromLists <$> many spaceParser `sepBy` eol

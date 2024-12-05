{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import AoC.Data.Grid.Rectangular
  ( Point,
    RectangularGrid (..),
    rotateClockwise,
    rotateCounterClockwise,
  )
import AoC.Parser (Parser, fromMap)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (eol)

data XMas = X | M | A | S deriving (Eq, Show)

xmasParser :: Parser XMas
xmasParser =
  fromMap $
    Map.fromList
      [ ("X", X),
        ("M", M),
        ("A", A),
        ("S", S)
      ]

puzzleParser :: Parser (RectangularGrid XMas)
puzzleParser =
  let puzzLine = Vector.fromList <$> some xmasParser
      puzzLines = (Vector.filter (not . Vector.null) . Vector.fromList <$> (puzzLine `sepEndBy` eol))
   in RectangularGrid <$> puzzLines

findSameSideXmas :: Point -> Vector.Vector (Vector.Vector XMas) -> Int
findSameSideXmas (col, row) mat =
  if mat ! row ! col == A
    && mat ! (row - 1) ! (col - 1) == S
    && mat ! (row - 1) ! (col + 1) == S
    && mat ! (row + 1) ! (col - 1) == M
    && mat ! (row + 1) ! (col + 1) == M
    then 1
    else 0

findFlipSideXmas :: Point -> Vector.Vector (Vector.Vector XMas) -> Int
findFlipSideXmas (col, row) mat =
  if mat ! row ! col == A
    && mat ! (row - 1) ! (col - 1) == S
    && mat ! (row + 1) ! (col + 1) == S
    && mat ! (row - 1) ! (col + 1) == M
    && mat ! (row + 1) ! (col - 1) == M
    then 1
    else 0

findXMasRight :: Point -> Vector.Vector (Vector.Vector XMas) -> Int
findXMasRight (col, row) mat =
  if mat ! row ! col == X
    && mat ! row ! (col + 1) == M
    && mat ! row ! (col + 2) == A
    && mat ! row ! (col + 3) == S
    then 1
    else 0

findXMasDiag :: Point -> Vector.Vector (Vector.Vector XMas) -> Int
findXMasDiag (col, row) mat =
  if mat ! row ! col == X
    && mat ! (row + 1) ! (col + 1) == M
    && mat ! (row + 2) ! (col + 2) == A
    && mat ! (row + 3) ! (col + 3) == S
    then 1
    else 0

showPuzz :: RectangularGrid XMas -> String
showPuzz (RectangularGrid mat) =
  let asList = Vector.toList (Vector.toList <$> mat)
      puzzLines =
        ( ( \case
              X -> 'X'
              M -> 'M'
              A -> 'A'
              S -> 'S'
          )
            <$>
        )
          <$> asList
   in unlines puzzLines

countXmas :: RectangularGrid XMas -> Int
countXmas (RectangularGrid mat) =
  let nRows = length mat
      nCols = length (mat ! 0)
      -- -4 because we're searching for XMas, so once we get too close to the edge,
      -- we don't need to look anymore
      pairs = [(col, row) | row <- [0 .. nRows - 1], col <- [0 .. nCols - 4]]
   in foldl'
        ( \acc coord ->
            acc
              + findXMasRight coord mat
              + ( if snd coord < nRows - 3 then findXMasDiag coord mat else 0
                )
        )
        0
        pairs

countMasXes :: RectangularGrid XMas -> Int
countMasXes (RectangularGrid mat) =
  let nRows = length mat
      nCols = length (mat ! 0)
      pairs = [(col, row) | row <- [1 .. nRows - 2], col <- [1 .. nCols - 2]]
   in foldl'
        ( \acc coord ->
            acc
              + findSameSideXmas coord mat
        )
        0
        pairs

solve1 :: RectangularGrid XMas -> Int
solve1 grid =
  countXmas grid
    + countXmas (rotateClockwise grid)
    + countXmas (rotateClockwise . rotateClockwise $ grid)
    + countXmas (rotateClockwise . rotateClockwise . rotateClockwise $ grid)

solve2 :: RectangularGrid XMas -> Int
solve2 grid =
  countMasXes grid
    + countMasXes (rotateClockwise grid)
    + countMasXes (rotateClockwise . rotateClockwise $ grid)
    + countMasXes (rotateCounterClockwise grid)

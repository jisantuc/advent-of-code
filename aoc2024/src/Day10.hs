{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import AoC.Data.Grid.Rectangular (Point, RectangularGrid (..), fromLists)
import AoC.Parser (Parser)
import Data.Foldable (foldMap')
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Data.Vector ((!))
import Text.Megaparsec (choice, many, sepBy)
import Text.Megaparsec.Char (eol)

data TraversalState = TraversalState
  { visited :: Set.Set Point,
    summits :: Set.Set Point
  }
  deriving (Eq, Show)

instance Semigroup TraversalState where
  (TraversalState {visited, summits}) <> (TraversalState {visited = visited', summits = summits'}) =
    TraversalState {visited = visited <> visited', summits = summits <> summits'}

instance Monoid TraversalState where
  mempty = TraversalState {visited = mempty, summits = mempty}

puzzleParser :: Parser (RectangularGrid Int)
puzzleParser =
  let rowParser =
        many $
          choice
            [ 0 <$ "0",
              1 <$ "1",
              2 <$ "2",
              3 <$ "3",
              4 <$ "4",
              5 <$ "5",
              6 <$ "6",
              7 <$ "7",
              8 <$ "8",
              9 <$ "9",
              99 <$ "."
            ]
   in fromLists <$> sepBy rowParser eol

scorePosition :: Int -> Int -> RectangularGrid Int -> Int
scorePosition row col (RectangularGrid puzz) =
  length . summits $ go row col mempty
  where
    nRows = length puzz
    nCols = length (puzz ! 0)
    validCoord (r, c) = r >= 0 && r < nRows && c >= 0 && c < nCols
    getHeight r c = puzz ! r ! c
    neighborCoords r c =
      filter
        validCoord
        [ (r - 1, c),
          (r + 1, c),
          (r, c - 1),
          (r, c + 1)
        ]
    neighbors r c =
      filter (\(r', c') -> getHeight r' c' - getHeight r c == 1) $ neighborCoords r c
    go r c state@(TraversalState {visited, summits}) =
      case (getHeight r c, neighbors r c) of
        (n, ns) ->
          let newBaseState =
                ( state
                    { summits = summits <> (if n == 9 then Set.singleton (r, c) else mempty),
                      visited = visited <> Set.singleton (r, c)
                    }
                )
           in newBaseState
                <> foldMap'
                  ( \(r', c') ->
                      go
                        r'
                        c'
                        newBaseState
                  )
                  (filter (`notElem` visited) ns)

solve1 :: RectangularGrid Int -> Int
solve1 puzz@(RectangularGrid grid) =
  let nRows = length grid
      nCols = length (grid ! 0)
      coords = [(r, c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
   in getSum $
        foldMap'
          ( \(r, c) ->
              case grid ! r ! c of
                0 -> Sum . scorePosition r c $ puzz
                _ -> mempty
          )
          coords

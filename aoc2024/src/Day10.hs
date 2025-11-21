{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 (scorePosition, puzzleParser, solve1) where

import AoC.Data.Grid.Rectangular (Point, RectangularGrid (..), fromLists)
import AoC.Parser (Parser)
import Data.Foldable (foldMap')
import qualified Data.Map.Strict as Map
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

validCoord :: Int -> Int -> (Int, Int) -> Bool
validCoord nRows nCols (r, c) = r >= 0 && r < nRows && c >= 0 && c < nCols

getHeight :: RectangularGrid a -> Int -> Int -> a
getHeight (RectangularGrid puzz) r c = puzz ! r ! c

neighborCoords :: RectangularGrid a -> Int -> Int -> [(Int, Int)]
neighborCoords (RectangularGrid puzz) r c =
  filter
    (validCoord (length puzz) (length $ puzz ! 0))
    [ (r - 1, c),
      (r + 1, c),
      (r, c - 1),
      (r, c + 1)
    ]


neighbors :: (Point -> Bool) -> RectangularGrid Int -> Int -> Int -> [Point]
neighbors cond puzz r c =  
  filter cond $ neighborCoords puzz r c

uphillNeighbors :: RectangularGrid Int -> Int -> Int -> [Point]
uphillNeighbors puzz r c =
  neighbors
    (\(r', c') -> getHeight puzz r' c' - getHeight puzz r c == 1)
    puzz
    r
    c

downhillNeighbors :: RectangularGrid Int -> Int -> Int -> [Point]
downhillNeighbors puzz r c =
  neighbors
    ( \(r', c') ->
        getHeight puzz r' c' - getHeight puzz r c == -1
    )
    puzz
    r
    c

scorePosition :: Int -> Int -> RectangularGrid Int -> Int
scorePosition row col puzz =
  length . summits $ go row col mempty
  where
    getHeight' = getHeight puzz
    neighbors' = uphillNeighbors puzz
    go r c state@(TraversalState {visited, summits}) =
      case (getHeight' r c, neighbors' r c) of
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

pathsToSummit :: Int -> Int -> RectangularGrid Int -> Map.Map Point (Sum Int)
pathsToSummit summitRow summitCol (RectangularGrid puzz) =
  undefined

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

solve2 :: RectangularGrid Int -> Int
solve2 _ = 3

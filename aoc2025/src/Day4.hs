{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Day4 where

import AoC.Data.Grid.Parser (sourcePositionToPoint)
import AoC.Data.Grid.Rectangular
  ( Point,
    RectangularGrid (..),
    fromLists,
    numCols,
    numRows,
  )
import qualified AoC.Parser as Parser
import Control.Monad (guard)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..), getSum)
import qualified Data.Set as Set
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Text.Megaparsec (getSourcePos, sepEndBy, some)
import Text.Megaparsec.Char (eol)

data FloorSpace = PaperRoll Point | Open Point deriving (Eq, Show)

location :: FloorSpace -> Point
location (PaperRoll p) = p
location (Open p) = p

isPaper :: FloorSpace -> Bool
isPaper (Open _) = False
isPaper (PaperRoll _) = True

debugSpace ::
  RectangularGrid [Point] ->
  RectangularGrid FloorSpace ->
  FloorSpace ->
  Char
debugSpace _ _ (Open _) = '.'
debugSpace neighbors spaces (PaperRoll p) =
  if checkAccessible neighbors spaces p then 'x' else '@'

puzzleParser :: Parser.Parser (RectangularGrid FloorSpace)
puzzleParser =
  let spaceParser = do
        p <- sourcePositionToPoint <$> getSourcePos
        constructor <- Parser.fromMap (Map.fromList [("@", PaperRoll), (".", Open)])
        pure $ constructor p
   in fromLists <$> (some spaceParser `sepEndBy` eol)

inGridNeighbors :: Int -> Int -> Point -> [Point]
inGridNeighbors rows cols p@(r, c) =
  filter
    ( \p'@(r', c') ->
        p' /= p
          && r' < rows
          && c' < cols
          && r' >= 0
          && c' >= 0
    )
    $ [(r + dR, c + dC) | dR <- [-1 .. 1], dC <- [-1 .. 1]]

countRolls :: Vector.Vector (Vector.Vector FloorSpace) -> [Point] -> Int
countRolls _ [] = 0
countRolls spaces ((r, c) : spaces') = case spaces ! r ! c of
  Open _ -> countRolls spaces spaces'
  PaperRoll _ -> 1 + countRolls spaces spaces'

solve1 :: RectangularGrid FloorSpace -> Int
solve1 grid@(RectangularGrid spaces) =
  let rows = numRows grid
      cols = numCols grid
      (RectangularGrid accessible) =
        (< 4) . countRolls spaces . (inGridNeighbors rows cols . location) <$> grid
   in getSum . mconcat $ do
        r <- [0 .. rows - 1]
        c <- [0 .. rows - 1]
        guard $ accessible ! r ! c && isPaper (spaces ! r ! c)
        pure $ Sum 1

checkAccessible :: RectangularGrid [Point] -> RectangularGrid FloorSpace -> Point -> Bool
checkAccessible (RectangularGrid neighbors) (RectangularGrid spaces') (r, c) =
  isPaper (spaces' ! r ! c) && countRolls spaces' (neighbors ! r ! c) < 4

solve2 :: RectangularGrid FloorSpace -> Int
solve2 grid =
  go 0 grid (Set.fromList $ [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]])
  where
    rows = numRows grid
    cols = numCols grid
    neighborsGrid@(RectangularGrid neighborsMat) = inGridNeighbors rows cols . location <$> grid
    go n grid'@(RectangularGrid spaces') pointsToCheck =
      let accessiblePoints = Set.filter (checkAccessible neighborsGrid grid') pointsToCheck
          byRow =
            foldl'
              ( \acc p@(r, c) ->
                  Map.unionWith (<>) acc (Map.singleton r [(c, Open p)])
              )
              Map.empty
              accessiblePoints
          newSpaces =
            RectangularGrid $
              Map.foldlWithKey
                ( \spacesGrid row rowUpdateVector ->
                    Vector.update spacesGrid [(row, Vector.update (spacesGrid ! row) rowUpdateVector)]
                )
                spaces'
                byRow
          neighborsToCheck = foldMap (Set.fromList . (\(r, c) -> neighborsMat ! r ! c)) accessiblePoints
       in if null accessiblePoints
            -- when there are no accessible points to check anymore, we're done, return the count
            then n
            else go (n + length accessiblePoints) newSpaces neighborsToCheck

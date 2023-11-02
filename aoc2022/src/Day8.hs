{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Day8 where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (swap)
import Data.Vector ((!))
import qualified Data.Vector as Vector

newtype Grid = Grid (Vector.Vector (Vector.Vector Int))
  deriving
    (Eq, Show)

fromText :: Text -> Grid
fromText input =
  let charList = Text.unpack <$> Text.lines input
      asInts = (digitToInt <$>) <$> charList
   in Grid $ Vector.fromList (Vector.fromList <$> asInts)

type Point = (Int, Int)

data FromSide = RightSide | LeftSide

data FromTop = Top | Bottom

atPoint :: Grid -> Point -> Int
atPoint (Grid v) (r, c)
  -- if the row index is outside the grid,
  -- or the column index is outside the row
  -- return -1, so all positive ints will be larger
  | r < 0 || r > length v || c < 0 || c > length (v ! r) = -1
  | otherwise = v ! r ! c

transpose :: Grid -> Grid
transpose g@(Grid v) =
  if null v
    then g
    else
      let nCols = length (v ! 0)
          columnIndices = Vector.fromList [0 .. nCols - 1]
       in Grid $ (\j -> (! j) <$> v) <$> columnIndices

visibleTreesFromSide :: FromSide -> Grid -> Int -> Set.Set Point
visibleTreesFromSide side g@(Grid v) rowIdx =
  let row = v ! rowIdx
      numCols = length row
      columnList = case side of
        LeftSide -> [0 .. numCols - 1]
        RightSide -> reverse [0 .. numCols - 1]
      indices = (rowIdx,) <$> columnList
   in snd $
        foldl'
          ( \(sup, acc) idx ->
              let value = g `atPoint` idx
               in if value > sup then (value, acc <> Set.singleton idx) else (sup, acc)
          )
          (-1, Set.empty)
          indices

visibleTreesFromTopBottom :: FromTop -> Grid -> Int -> Set.Set Point
visibleTreesFromTopBottom Top g =
  Set.map swap
    . visibleTreesFromSide LeftSide (transpose g)
visibleTreesFromTopBottom Bottom g =
  Set.map swap
    . visibleTreesFromSide RightSide (transpose g)

countVisibleTrees :: Int -> Vector.Vector Int -> Int
countVisibleTrees maxHeight neighbors =
  let zero = (-1, 0)
   in snd $
        Vector.foldl'
          ( \acc@(maxSeen, count) tree ->
              if maxSeen >= maxHeight
                then acc
                else
                  if tree <= maxHeight
                    || ( maxSeen < maxHeight
                           && tree >= maxHeight
                       )
                    then (tree `max` maxSeen, count + 1)
                    else (maxSeen, count)
          )
          zero
          neighbors

visibleTreesToRight :: Point -> Grid -> Int
visibleTreesToRight p@(r, c) g@(Grid v) =
  let row = v ! r
      pastIdx = Vector.drop (c + 1) row
      maxHeight = g `atPoint` p
   in countVisibleTrees maxHeight pastIdx

visibleTreesBelow :: Point -> Grid -> Int
visibleTreesBelow p g =
  visibleTreesToRight (swap p) (transpose g)

visibleTreesToLeft :: Point -> Grid -> Int
visibleTreesToLeft p@(r, c) g@(Grid v) =
  let row = v ! r
      rowUpToColumn = Vector.take c row
      reversed = Vector.reverse rowUpToColumn
      maxHeight = g `atPoint` p
   in countVisibleTrees maxHeight reversed

visibleTreesAbove :: Point -> Grid -> Int
visibleTreesAbove p g = visibleTreesToLeft (swap p) (transpose g)

scenicScore :: Grid -> Point -> Int
scenicScore g p =
  visibleTreesAbove p g
    * visibleTreesBelow p g
    * visibleTreesToRight p g
    * visibleTreesToLeft p g

solvePart1 :: Grid -> Int
solvePart1 g@(Grid v) =
  let nRows = length v
      nCols = length (v ! 0)
      rowIndices = Set.fromList [0 .. nRows - 1]
      colIndices = Set.fromList [0 .. nCols - 1]
   in length $
        foldl'
          ( \acc idx ->
              acc
                <> visibleTreesFromSide LeftSide g idx
                <> visibleTreesFromSide RightSide g idx
          )
          Set.empty
          rowIndices
          <> foldl'
            ( \acc idx ->
                acc
                  <> visibleTreesFromTopBottom Top g idx
                  <> visibleTreesFromTopBottom Bottom g idx
            )
            Set.empty
            colIndices

solvePart2 :: Grid -> Int
solvePart2 g@(Grid v) =
  let nRows = length v
      nCols = length $ v ! 0
      pointGrid = (,) <$> [0 .. nRows - 1] <*> [0 .. nCols - 1]
      scores = parMap rpar (scenicScore g) pointGrid
   in maximum scores

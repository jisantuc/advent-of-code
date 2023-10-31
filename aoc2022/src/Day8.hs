{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}

module Day8 where

import Data.Foldable (foldl')
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Vector ((!))
import qualified Data.Vector as Vector

-- list of rows --
newtype Grid = Grid (Vector.Vector (Vector.Vector Int))
  deriving
    (Eq, Show)

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

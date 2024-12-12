module Day8 where

import AoC.Data.Grid.Parser (sourcePositionToPoint)
import AoC.Data.Grid.Rectangular (Point)
import AoC.Parser (Parser)
import Data.Functor (($>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec (MonadParsec (eof), getSourcePos, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol)

type AntennaMap = Map.Map Char (Set.Set Point)

puzzleParser :: (Int, Int) -> AntennaMap -> Parser ((Int, Int), AntennaMap)
puzzleParser bounds@(maxRow, maxCol) acc =
  (eof $> (bounds, acc))
    <|> (eol *> puzzleParser bounds acc)
    <|> ( getSourcePos
            >>= ( \(row, col) ->
                    let newBounds = (maxRow `max` row, maxCol `max` col)
                     in (char '.' *> puzzleParser newBounds acc)
                          <|> ( alphaNumChar
                                  >>= \c ->
                                    puzzleParser
                                      newBounds
                                      (Map.unionWith (<>) acc (Map.singleton c (Set.singleton (row, col))))
                              )
                )
              . sourcePositionToPoint
        )

antinodesForPair :: Point -> Point -> [Point]
antinodesForPair (r1, c1) (r2, c2) =
  let dx = c2 - c1
      dy = r2 - r1
   in [(r1 - dy, c1 - dx), (r2 + dy, c2 + dx)]

antinodesForPair2 :: Int -> Int -> Point -> Point -> Set.Set Point
antinodesForPair2 maxRow maxCol (r1, c1) (r2, c2) =
  let colDiff = c2 - c1
      rowDiff = r2 - r1
      reduce rise run
        | rise == run = (1, 1)
        | rise `mod` run == 0 = (rise `div` run, 1)
        | run `mod` rise == 0 = (1, run `div` rise)
        | otherwise = (rise, run)
      (dy, dx) = reduce rowDiff colDiff
      inBounds (r, c) = r >= 0 && r <= maxRow && c >= 0 && c <= maxCol
   in Set.fromList $
        takeWhile inBounds ((\idx -> (r1 + idx * dy, c1 + idx * dx)) <$> [0 ..])
          <> takeWhile inBounds ((\idx -> (r1 - idx * dy, c1 - idx * dx)) <$> [0 ..])

antinodesForFrequency :: Set.Set Point -> Set.Set Point
antinodesForFrequency points =
  let ps = Set.toList points
      allPairs = [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]
   in Set.fromList $ allPairs >>= uncurry antinodesForPair

antinodesForFrequency2 :: Int -> Int -> Set.Set Point -> Set.Set Point
antinodesForFrequency2 maxRow maxCol points =
  let ps = Set.toList points
      allPairs = Set.fromList $ [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]
   in foldMap (uncurry (antinodesForPair2 maxRow maxCol)) allPairs

solve1 :: Point -> AntennaMap -> Int
solve1 (maxRow, maxCol) =
  Set.size
    . Set.filter (\(r, c) -> r >= 0 && r <= maxRow && c >= 0 && c <= maxCol)
    . Map.foldl' (\acc points -> Set.union acc (antinodesForFrequency points)) Set.empty

solve2 :: Point -> AntennaMap -> Int
solve2 (maxRow, maxCol) =
  Set.size
    . Map.foldl' (\acc points -> Set.union acc (antinodesForFrequency2 maxRow maxCol points)) Set.empty

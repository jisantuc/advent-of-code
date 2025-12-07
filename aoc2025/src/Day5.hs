{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import AoC.Parser (Parser)
import Data.Foldable (foldMap', foldl')
import Data.Interval (IntervalLit (..), containedBy, coveredBy)
import Data.IntervalIndex (IntervalIndex, allIntervals, at, fromList, insert, touching)
import Data.Monoid (Sum (..))
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol, spaceChar)
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = Puzzle
  { freshRanges :: IntervalIndex Int (IntervalLit Int),
    ingredients :: [Int]
  }
  deriving (Eq, Show)

puzzleParser :: Parser Puzzle
puzzleParser = do
  ranges <-
    fromList
      <$> (IntervalLit <$> (decimal <* "-") <*> ((+ 1) <$> decimal))
        `sepEndBy` eol
  _ <- spaceChar
  ingredients' <- decimal `sepEndBy` eol
  pure $ Puzzle ranges ingredients'

solve1 :: Puzzle -> Int
solve1 (Puzzle {freshRanges, ingredients}) =
  getSum $
    foldMap'
      ( \ingredient ->
          if null (freshRanges `at` ingredient) then Sum 0 else Sum 1
      )
      ingredients

solve2 :: Puzzle -> Int
solve2 (Puzzle {freshRanges}) =
  let deconflictWith :: IntervalLit Int -> IntervalLit Int -> [IntervalLit Int]
      deconflictWith interval1 interval2
        -- no overlap
        | start interval2 >= end interval1 || end interval2 <= start interval1 = [interval1]
        -- candidate interval fully covered
        | interval1 `coveredBy` interval2 = []
        | start interval1 `containedBy` interval2 = [interval1 {start = end interval2}]
        | end interval1 `containedBy` interval2 = [interval1 {end = start interval2}]
        -- interval2 must be in the middle of interval 1 -- chop around it
        | otherwise = [interval1 {end = start interval2}, interval1 {start = end interval2}]
   in snd $
        foldl'
          ( \(index, uniqueElems) interval@(IntervalLit {start, end}) ->
              let conflicting = index `touching` interval
               in if null conflicting
                    then
                      (index `insert` interval, uniqueElems + end - start)
                    else
                      let deconflicted =
                            foldl'
                              ( \acc conflict ->
                                  foldMap (`deconflictWith` conflict) acc
                              )
                              [interval]
                              conflicting
                          newElems =
                            getSum $
                              foldMap' (\(IntervalLit start' end') -> Sum (end' - start')) deconflicted
                       in (index `insert` interval, uniqueElems + newElems)
          )
          (mempty, 0)
          (allIntervals freshRanges)

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import AoC.Parser (Parser)
import Data.Foldable (foldMap')
import Data.Functor (void)
import Data.Interval (IntervalLit (..))
import Data.Monoid (Sum (..))
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char.Lexer (decimal)

puzzleParser :: Parser [IntervalLit Integer]
puzzleParser =
  ( do
      start' <- decimal
      void "-"
      IntervalLit start' <$> decimal
  )
    `sepEndBy` ","

solve1 :: [IntervalLit Integer] -> Integer
solve1 =
  getSum
    . foldMap
      ( \(IntervalLit {start, end}) ->
          let nums = [start .. end]
              match num =
                let asString = show num
                    (h, t) = splitAt (length asString `div` 2) asString
                 in h == t
              invalidIds = filter match nums
           in mconcat . (Sum <$>) $ invalidIds
      )

solve2 :: [IntervalLit Integer] -> Integer
solve2 =
  getSum
    . foldMap'
      ( \(IntervalLit {start, end}) ->
          let nums = [start .. end]
              chunksOf 0 s = [s]
              chunksOf _ [] = []
              chunksOf n s =
                let (h, t) = splitAt n s
                 in h : chunksOf n t
              allEq [] = True
              allEq [_] = False
              allEq (x : xs) = all (== x) xs
              go groupSize s =
                let maxSize = length s `div` 2
                    chunks = chunksOf groupSize s
                 in allEq chunks || ((groupSize < maxSize) && go (groupSize + 1) s)
              invalidIds = filter (go 1 . show) nums
           in foldMap' Sum invalidIds
      )

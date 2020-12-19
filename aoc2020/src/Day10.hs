module Day10 where

import Data.List (foldl')
import Data.Semigroup (Product (Product))
import qualified Data.Vector as V
import Parser (Parser)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Solution = Solution
  { threeJoltDiffs :: Int,
    oneJoltDiffs :: Int,
    mystery :: Int
  }
  deriving (Show)

emptySol :: Solution
emptySol = Solution 0 0 0

joltDifferences :: V.Vector Int -> V.Vector Int
joltDifferences nums = V.zipWith (-) (V.drop 1 nums) nums

solve :: [Int] -> Solution
solve nums =
  let folder acc num = case num of
        1 -> acc {oneJoltDiffs = oneJoltDiffs acc + 1}
        3 -> acc {threeJoltDiffs = threeJoltDiffs acc + 1}
        _ -> acc {mystery = mystery acc + 1}
   in foldl' folder emptySol nums

puzzleParser :: Parser [Int]
puzzleParser = (0 :) <$> sepBy decimal eol

splitOn :: Int -> V.Vector Int -> (V.Vector Int, V.Vector Int)
splitOn n vec =
  (V.takeWhile (/= n) vec, V.dropWhile (== n) $ V.dropWhile (/= n) vec)

contiguousOnes :: V.Vector Int -> [Int]
contiguousOnes diffs =
  let (h, t) = splitOn 3 diffs
   in if (V.null t) then [length h] else (length h : contiguousOnes t)

pathsForContiguousRegionSize :: Int -> Int
pathsForContiguousRegionSize x
  | x == 4 = 7
  | x == 3 = 4
  | x == 2 = 2
  | otherwise = 1

countArrangements :: V.Vector Int -> Int
countArrangements differences =
  let regions = contiguousOnes differences
   in foldl' (*) 1 $ pathsForContiguousRegionSize <$> regions

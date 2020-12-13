module Day10 where

import Data.List (foldl')
import Debug.Trace (trace)
import Parser (Parser)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Solution = Solution
  { threeVoltDiffs :: Int,
    oneVoltDiffs :: Int,
    mystery :: Int
  }
  deriving (Show)

emptySol :: Solution
emptySol = Solution 0 0 0

voltDifferences :: [Int] -> [Int]
voltDifferences nums = trace (show nums) $ zipWith (-) (drop 1 nums) nums

solve :: [Int] -> Solution
solve nums =
  let folder acc num = case num of
        1 -> acc {oneVoltDiffs = oneVoltDiffs acc + 1}
        3 -> acc {threeVoltDiffs = threeVoltDiffs acc + 1}
        _ -> acc {mystery = mystery acc + 1}
   in foldl' folder emptySol nums

puzzleParser :: Parser [Int]
puzzleParser = sepBy decimal eol

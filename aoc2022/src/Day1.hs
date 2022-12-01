module Day1 where

import Data.Foldable (foldl')
import Parser (Parser)
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

type CalorieCount = Int

type Knapsack = [CalorieCount]

type Puzzle = [Knapsack]

knapsackParser :: Parser Knapsack
knapsackParser =
  decimal `sepEndBy` eol

puzzleParser :: Parser Puzzle
puzzleParser =
  knapsackParser `sepBy` eol

solvePart1 :: Puzzle -> Int
solvePart1 =
  foldl' (\acc cals -> let total = sum cals in max total acc) 0

insertItem :: Ord a => [a] -> a -> [a]
insertItem [] item = [item]
insertItem items@(h : t) item =
  if item > h then item : items else h : insertItem t item

-- as we iterate through the knapsacks
-- for each elf's knapsack
-- if acc is empty, singelton it up
-- otherwise, insert it into its place
solvePart2 :: Puzzle -> Int
solvePart2 puzz =
  let sums = sum <$> puzz
   in sum . take 3 $ foldl' insertItem [] sums

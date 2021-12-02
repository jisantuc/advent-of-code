module Day1 where

import Data.Foldable (Foldable (foldl'))
import Data.List (tails, transpose)
import Parser (Parser)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Puzzle = [Int]

puzzleParser :: Parser Puzzle
puzzleParser = decimal `sepEndBy` eol

solvePart1 :: Puzzle -> Int
solvePart1 [] = 0
solvePart1 ints@(_ : t) =
  foldl' (\acc x -> if (x > 0) then acc + 1 else acc) 0 $ zipWith (-) t ints

slidingSums :: Int -> [Int] -> [Int]
slidingSums windowSize ns =
  -- https://twitter.com/GabriellaG439/status/701460899589017600
  let sliding = transpose (take windowSize (tails ns))
   in foldl' (+) 0 <$> sliding

solvePart2 :: Puzzle -> Int
solvePart2 ns = solvePart1 $ slidingSums 3 ns

solve :: Puzzle -> Int
solve = solvePart2

module Day1 where
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Parser (Parser)
import Text.Megaparsec (sepEndBy)
import Data.Foldable (Foldable(foldl'))
import Data.List (transpose, tails)

type Puzzle = [Int]

puzzleParser :: Parser Puzzle
puzzleParser = decimal `sepEndBy` eol

solvePart1 :: Puzzle -> Int
solvePart1 [] = 0
solvePart1 ints@(_:t) =
    foldl' (\acc x -> if (x > 0) then acc + 1 else acc) 0 $ zipWith (-) t ints

slidingSums :: Int -> [Int] -> [Int]
slidingSums windowSize ns =
    let sliding = transpose (take windowSize (tails ns))
    in
        foldl' (+) 0 <$> sliding

solvePart2 :: Puzzle -> Int
solvePart2 ns = solvePart1 $ slidingSums 3 ns

solve :: Puzzle -> Int
solve = solvePart2
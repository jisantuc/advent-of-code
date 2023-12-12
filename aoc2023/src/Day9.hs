module Day9 where

import AoC.Parser (Parser)
import Data.Foldable (Foldable (foldMap'))
import Data.Monoid (Sum (..))
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal, signed)

newtype Puzzle = Puzzle [[Int]] deriving (Eq, Show)

parser :: Parser Puzzle
parser =
  Puzzle <$> ((signed hspace decimal `sepBy` hspace) `sepEndBy` eol)

findNextValue :: [Int] -> Int
findNextValue xs =
  if all (== 0) xs
    then 0
    else last xs + findNextValue (zipWith (-) (tail xs) xs)

solver1 :: Puzzle -> Int
solver1 (Puzzle lists) =
  getSum $ foldMap' (Sum . findNextValue) lists

solver2 :: Puzzle -> Int
solver2 (Puzzle lists) = solver1 (Puzzle $ reverse <$> lists)


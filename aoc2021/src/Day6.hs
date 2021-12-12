module Day6 where

import Data.Foldable (foldl')
import Data.List (genericLength)
import qualified Data.Map.Strict as M
import Data.Monoid (Sum (..))
import Parser (Parser)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

newtype Lanternfish = Lanternfish {daysRemaining :: Integer} deriving (Eq, Ord, Show)

type Puzzle = [Lanternfish]

lanternfishParser :: Parser Lanternfish
lanternfishParser = Lanternfish <$> decimal

puzzleParser :: Parser Puzzle
puzzleParser = lanternfishParser `sepBy` (char ',')

step :: Lanternfish -> [Lanternfish]
step (Lanternfish 0) = [Lanternfish 6, Lanternfish 8]
step (Lanternfish n) = [Lanternfish (n - 1)]

-- need a smarter algorithm here, since keeping all of the fish with different
-- remaining days together requires going one by one through steps
-- instead:
--   - partition by values
--   - split on reproduction
--   - maybe... get lots of reuse from GHC? fingers crossed
fasterSum :: Integer -> [Lanternfish] -> Sum Integer
fasterSum 0 puzz = genericLength puzz
fasterSum n [Lanternfish 0] =
  fasterSum (n - 1) [Lanternfish 8] + fasterSum (n - 1) [Lanternfish 6]
fasterSum n [Lanternfish d]
  | n > d = fasterSum (n - d) [Lanternfish 0]
  | n == d = 1
  | n < d = 1
fasterSum n xs =
  let valueCounts = countValues xs
   in foldl' (\acc (k, v) -> acc + v * fasterSum n [k]) 0 $ M.toList valueCounts

countValues :: Puzzle -> M.Map Lanternfish (Sum Integer)
countValues =
  foldl' (\acc n -> M.unionWith (<>) acc (M.singleton n (Sum 1))) M.empty

solvePart1 :: Puzzle -> Integer
solvePart1 = getSum . fasterSum 80

solvePart2 :: [Lanternfish] -> Integer
solvePart2 = getSum . fasterSum 256

solve :: Puzzle -> Integer
solve = solvePart1

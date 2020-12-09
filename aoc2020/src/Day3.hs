module Day3 where

import Data.Monoid (Product)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, (<|>))
import Text.Megaparsec.Char (char, eol)

type Parser = Parsec Void Text

data GridSquare = Tree | OpenSquare deriving (Eq, Show)

type Row = [GridSquare]

lineParser :: Parser [GridSquare]
lineParser =
  (many $ (Tree <$ char '#') <|> (OpenSquare <$ char '.')) <* eol

puzzleParser :: Parser [Row]
puzzleParser = many lineParser

trajectories :: [(Int, Int)]
trajectories =
  [ (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  ]

countTrees :: (Int, Int) -> [Row] -> Product Int
countTrees traj grid =
  let indexed = zip [0 ..] grid
      checkTree :: (Int, Int) -> (Int, Row) -> GridSquare
      checkTree (deltaX, deltaY) (idx, row) =
        if (deltaY == 0 || (idx `mod` deltaY) == 0)
          then cycle row !! (idx * deltaX `div` deltaY)
          else OpenSquare
   in foldl (\s r -> if (checkTree traj r == Tree) then s + 1 else s) 0 indexed

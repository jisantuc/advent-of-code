module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day1 (puzzleParser, solvePart2)
import Parser (parsePuzzle)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let result = Day1.solvePart2 <$> parsePuzzle Day1.puzzleParser puzzle
  print result

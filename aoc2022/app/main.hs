module Main (main) where

import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day7 (puzzleParser)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let parsed = parsePuzzle puzzleParser puzzle
  print $ either errorBundlePretty (const "Parsing successful!") parsed
  let (Right instructions) = parsed
  result <- solvePart2 puzz instructions
  print result

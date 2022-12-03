module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day2 (puzzleParser2, solvePart2)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let parsed = parsePuzzle puzzleParser2 puzzle
  print $ either errorBundlePretty (const "Parsing successful!") parsed
  let result = solvePart2 <$> parsed
  print result

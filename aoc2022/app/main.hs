module Main (main) where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day6 (solvePart2)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  -- let parsed = parsePuzzle puzzleParser puzzle
  -- print $ either errorBundlePretty (const "Parsing successful!") parsed
  let result = solvePart2 $ T.unpack puzzle
  print result

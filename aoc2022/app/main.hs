module Main (main) where

import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day8 (fromText, solvePart2)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let parsed = fromText puzzle
  print $ solvePart2 parsed

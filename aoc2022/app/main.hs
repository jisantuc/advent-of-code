module Main (main) where

import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day5 (fullParser, solvePart2)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let Right (puzzIO, instructions) = parsePuzzle fullParser puzzle
  puzz <- puzzIO
  -- print $ either errorBundlePretty (const "Parsing successful!") parsed
  result <- solvePart2 puzz instructions
  print result

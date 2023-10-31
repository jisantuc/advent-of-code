module Main (main) where

import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Day7 (buildState, puzzleParser, solvePart2)
import Parser (parsePuzzle)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  puzzle <- Text.readFile "puzzle.txt"
  let parsed = parsePuzzle puzzleParser puzzle
  print $ either errorBundlePretty (const "Parsing successful!") parsed
  case parsed of
    (Right instructions) -> do
      puzzleState <- buildState instructions
      result <- solvePart2 puzzleState
      print $ "Result " <> show result
    _ -> print "oh no"

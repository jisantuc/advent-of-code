module Main where

import qualified Data.Text.IO as TextIO
import Day6 (puzzleParser, solve)
import Parser (parsePuzzle)

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = parsePuzzle puzzleParser puzzleInput
  case parseResult of
    Right puzz ->
      do
        print $ show "Population size: " ++ (show $ solve puzz)
    Left err ->
      do
        print "oh no"
        print . show $ err

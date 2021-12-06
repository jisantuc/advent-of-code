module Main where

import qualified Data.Text.IO as TextIO
import Day4 (boards, puzzleParser, solve)
import Parser (parsePuzzle)

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = parsePuzzle puzzleParser puzzleInput
  case parseResult of
    Right puzz ->
      do
        print $ "How many puzzles: " ++ (show $ length . boards $ puzz)
        print $ "The last puzzle: " ++ (show $ boards puzz !! 99)
        print $ show "Bingo state: " ++ (show $ solve puzz)
    Left err ->
      do
        print "oh no"
        print . show $ err

module Main where

import Data.List (sort)
import qualified Data.Text.IO as TextIO
import Day10 (oneVoltDiffs, puzzleParser, solve, threeVoltDiffs, voltDifferences)
import Text.Megaparsec (ParseErrorBundle, Parsec, parseTest, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  -- parseTest puzzleParser puzzleInput
  let parseResult = parsePuzzle puzzleParser puzzleInput
  case parseResult of
    Right puzz ->
      let solution = solve . voltDifferences . sort $ puzz
       in print . show $ (oneVoltDiffs solution + 1) * (threeVoltDiffs solution + 1)
    Left err ->
      do
        print "oh no 😢"
        print . show $ err

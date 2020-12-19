module Main where

import Data.List (sort)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as V
import Day10 (countArrangements, joltDifferences, oneJoltDiffs, puzzleParser, solve, threeJoltDiffs)
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
      let sorted = sort puzz
          diffs = joltDifferences (V.fromList sorted)
       in do
            print . show $ diffs
            let puzzleVec = V.fromList $ sort puzz
            print . show $ countArrangements diffs
    Left err ->
      do
        print "oh no 😢"
        print . show $ err

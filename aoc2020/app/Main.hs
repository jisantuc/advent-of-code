module Main where

import Data.List (sort)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as V
import Day11 (Seat (..), countOccupied, countWhen, puzzleParser, solve)
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
      do
        print $ show "Occupied seats: " ++ (show $ solve puzz)
    Left err ->
      do
        print "oh no 😢"
        print . show $ err

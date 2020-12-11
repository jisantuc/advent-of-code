module Main where

import qualified Data.Text.IO as TextIO
import Day5 (puzzleParser, seatId)
import Text.Megaparsec (ParseErrorBundle, Parsec, parseTest, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let seats = parsePuzzle puzzleParser puzzleInput
  print $ show . maximum . (seatId <$>) <$> seats

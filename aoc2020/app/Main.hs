module Main where

import qualified Data.Text.IO as TextIO
import Day9 (puzzleParser, solve)
import Text.Megaparsec (ParseErrorBundle, Parsec, parseTest, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  parseTest puzzleParser puzzleInput
  let parseResult = parsePuzzle puzzleParser puzzleInput
  print . show $ solve 25 25 <$> parseResult

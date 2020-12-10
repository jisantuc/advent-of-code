module Main where

import qualified Data.Text.IO as TextIO
import Day4 (isValid, puzzleParser, validate)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  print $ show . length . filter isValid . (validate <$>) <$> parsePuzzle puzzleParser puzzleInput

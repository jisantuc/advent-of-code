module Main where

import qualified Data.Text.IO as TextIO
import Day2 (isValid, puzzleParser)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = parsePuzzle puzzleParser puzzleInput
  print $ ("Valid lines: " ++) . show . length . filter isValid <$> parseResult

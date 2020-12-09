module Main where

import qualified Data.Text.IO as TextIO
import Day3 (countTrees, puzzleParser, trajectories)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = parsePuzzle puzzleParser puzzleInput
  let counts = (countTrees <$> trajectories <*>) <$> ((: []) <$> parseResult)
  print . show $ mconcat <$> counts

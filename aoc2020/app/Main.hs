module Main where

import qualified Data.Set as Set
import qualified Data.Text.IO as TextIO
import Day7 (findContainersFor, puzzleParser, shinyGold)
import Text.Megaparsec (ParseErrorBundle, Parsec, runParser)

parsePuzzle :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parsePuzzle parser puzzle = runParser parser "" puzzle

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = parsePuzzle puzzleParser puzzleInput
  case parseResult of
    Left err -> print $ show err
    Right policy ->
      do
        print . show . Set.size $ findContainersFor shinyGold policy

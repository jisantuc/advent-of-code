module Main where

import Data.List (sort)
import qualified Data.Text.IO as TextIO
import qualified Data.Vector as V
import Text.Megaparsec (ParseErrorBundle, Parsec, parseTest, runParser)

puzzleParser :: Parsec e s a
puzzleParser = undefined

solve :: Show a => s -> a
solve = undefined

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
        print $ show "First successful ts: " ++ (show $ (solve puzz :: Int))
    Left err ->
      do
        print "oh no 😢"
        print . show $ err
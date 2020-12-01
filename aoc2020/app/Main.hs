module Main where

import qualified Data.Text.IO as TextIO
import Day1 ( find32020, puzzleParser, tupProduct3 )
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  puzzleInput <- TextIO.readFile "puzzle.txt"
  let parseResult = runParser puzzleParser "" puzzleInput
  let result = (find32020 <$> parseResult)
  print $ (tupProduct3 <$>) <$> result

module Lib
  ( Instruction(..)
  , interpretInstructions
  , manhattanDistance
  , puzzleLineParser
  ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8
import           Data.Sequence

type Point = (Int, Int)

type History = Seq Point

data Instruction
  = Right' Int
  | Left' Int
  | Up Int
  | Down Int

getParser :: (Int -> Instruction) -> Char -> Parser Instruction
getParser f c = f <$> (char c *> decimal)

instParser :: Parser Instruction
instParser = do
  getParser Right' 'R' <|> getParser Left' 'L' <|> getParser Up 'U' <|>
    getParser Down 'D'

puzzleLineParser :: Parser [Instruction]
puzzleLineParser = sepBy instParser (char ',') <* endOfLine

interpretInstructions :: History -> [Instruction] -> History
interpretInstructions Empty instrs =
  interpretInstructions (singleton (0, 0)) instrs
interpretInstructions hist [] = hist
interpretInstructions hist@(_ :|> (x, y)) (inst:instrs) =
  case inst of
    Right' 0 -> interpretInstructions hist instrs
    Left' 0 -> interpretInstructions hist instrs
    Up 0 -> interpretInstructions hist instrs
    Down 0 -> interpretInstructions hist instrs
    Right' n ->
      interpretInstructions (hist |> (x + 1, y)) (Right' (n - 1) : instrs)
    Left' n ->
      interpretInstructions (hist |> (x - 1, y)) (Left' (n - 1) : instrs)
    Up n -> interpretInstructions (hist |> (x, y + 1)) (Up (n - 1) : instrs)
    Down n -> interpretInstructions (hist |> (x, y - 1)) (Down (n - 1) : instrs)

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

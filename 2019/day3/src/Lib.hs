module Lib
  ( Instruction(..)
  , interpretInstructions
  ) where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Char8
import           Data.Sequence

import           Debug.Trace                      (trace)

type Point = (Int, Int)
type History = Seq Point

data Instruction
  = Right' Int
  | Left' Int
  | Up Int
  | Down Int

getParser :: (Int -> Instruction) -> Char -> Parser Instruction
getParser f c = f <$> (char c *> decimal)

parseInstruction :: Parser Instruction
parseInstruction = do
  getParser Right' 'R' <|> getParser Left' 'L' <|> getParser Up 'U' <|>
    getParser Down 'D'

interpretInstructions :: [Instruction] -> History -> ([Instruction], History)
interpretInstructions instrs Empty = interpretInstructions instrs (singleton (0, 0))
interpretInstructions [] hist = ([], hist)
interpretInstructions (inst:instrs) hist@(_ :|> (x, y)) =
  case inst of
    Right' 0 ->
      interpretInstructions instrs hist
    Left' 0 ->
      interpretInstructions instrs hist
    Up 0 ->
      interpretInstructions instrs hist
    Down 0 ->
      interpretInstructions instrs hist
    Right' n ->
      interpretInstructions (Right' (n - 1) : instrs) (hist |> (x + 1, y))
    Left' n ->
      interpretInstructions (Left' (n - 1) : instrs) (hist |> (x - 1, y))
    Up n ->
      interpretInstructions (Up (n - 1) : instrs) (hist |> (x, y + 1))
    Down n ->
      interpretInstructions (Down (n - 1) : instrs) (hist |> (x, y - 1))

module Lib
    ( instParser
    , solve
    , Instruction (..)
    ) where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString.Char8 hiding (take)
import           Debug.Trace                      (trace)

type Puzzle = [Int]
type OutBuffer = Int
type InRegister = Int
data Instruction = Add InRegister InRegister OutBuffer
  | Multiply InRegister InRegister OutBuffer
  | Halt
  | Invalid Int deriving (Eq, Show)

getInstParser :: (InRegister -> InRegister -> OutBuffer -> Instruction) -> Char -> Parser Instruction
getInstParser f c = do
  _ <- char c <* char ','
  inReg1 <- decimal <* char ','
  inReg2 <- decimal <* char ','
  outBuffer <- decimal
  return $ f inReg1 inReg2 outBuffer

addParser :: Parser Instruction
addParser = getInstParser Add '1'

multParser :: Parser Instruction
multParser = getInstParser Multiply '2'

haltParser :: Parser Instruction
haltParser =
  const Halt <$> string "99"

instParser :: Parser Instruction
instParser =
  addParser <|> multParser <|> haltParser <|> Invalid <$> decimal

updatePuzz :: (Int -> Int -> Int) -> Int -> Int -> Int -> Puzzle -> Puzzle
updatePuzz f reg1 reg2 out puzz =
  let
    newValue = f (puzz !! reg1) (puzz !! reg2)
    newPuzz =
      case out of
        0 ->
          newValue : drop 1 puzz
        n ->
          take n puzz ++ [newValue] ++ drop (n + 1) puzz
  in
    newPuzz

-- debug left in because I don't feel like returning Eithers
interpret :: Puzzle -> Instruction -> Puzzle
interpret puzz Halt = puzz
interpret puzz (Add reg1 reg2 out) =
  updatePuzz (+) reg1 reg2 out puzz
interpret puzz (Multiply reg1 reg2 out) =
  updatePuzz (*) reg1 reg2 out puzz
interpret puzz (Invalid _) =
  trace "Really shouldn't be here -- should halt before having to interpret any invalid commands" puzz

-- debug left in because I don't feel like returning Eithers
solve :: Puzzle -> [Instruction] -> Puzzle
solve puzz [] = trace "Really should have halted before exhausting the instruction list" puzz
solve puzz (Halt:_) = puzz
solve puzz (inst:instrs) = solve (interpret puzz inst) instrs

{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Data.Set as Set
import Data.Text (Text)
import Parser (Parser)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction
  = NoOp
  | Jump Int
  | Accumulate Int
  deriving (Eq, Show)

type AccValue = Int

data HandheldPuzzle = HandheldPuzzle
  { executedLines :: Set.Set Int,
    instructionSet :: [Instruction],
    nextLine :: Int,
    accValue :: AccValue
  }
  deriving (Show)

signedInt :: Parser Int
signedInt = signed (pure ()) decimal

parseInst :: Text -> (Int -> Instruction) -> Parser Instruction
parseInst symbol f =
  f <$> (string symbol *> char ' ' *> signedInt)

instructionParser :: Parser Instruction
instructionParser =
  do
    parseInst "nop" (pure NoOp)
    <|> parseInst "jmp" Jump
    <|> parseInst "acc" Accumulate

update :: HandheldPuzzle -> Instruction -> HandheldPuzzle
update puzz NoOp =
  puzz
    { nextLine = nextLine puzz + 1,
      executedLines = Set.union (Set.singleton $ nextLine puzz) (executedLines puzz)
    }
update puzz (Jump x) =
  puzz
    { nextLine = nextLine puzz + x,
      executedLines = Set.union (Set.singleton $ nextLine puzz) (executedLines puzz)
    }
update puzz (Accumulate x) =
  puzz
    { nextLine = nextLine puzz + 1,
      accValue = accValue puzz + x,
      executedLines = Set.union (Set.singleton $ nextLine puzz) (executedLines puzz)
    }

solvePuzzle :: HandheldPuzzle -> AccValue
solvePuzzle puzz =
  let instruction = instructionSet puzz !! nextLine puzz
      newPuzz = update puzz instruction
      doContinue = not $ nextLine newPuzz `elem` executedLines puzz
   in if (doContinue) then solvePuzzle newPuzz else accValue newPuzz

puzzleParser :: Parser HandheldPuzzle
puzzleParser =
  (\instructions -> HandheldPuzzle Set.empty instructions 0 0)
    <$> sepBy instructionParser eol

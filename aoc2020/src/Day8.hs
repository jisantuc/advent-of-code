{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Control.Applicative (Applicative (liftA2))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector.Generic ((!?))
import Parser (Parser)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction
  = NoOp Int
  | Jump Int
  | Accumulate Int
  deriving (Eq, Show)

data HaltCause = LoopDetected | Termination deriving (Show)

type AccValue = Int

data HandheldPuzzle = HandheldPuzzle
  { executedLines :: Set.Set Int,
    instructionSet :: V.Vector Instruction,
    nextLine :: Int,
    accValue :: AccValue,
    haltCause :: Maybe HaltCause
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
    parseInst "nop" NoOp
    <|> parseInst "jmp" Jump
    <|> parseInst "acc" Accumulate

update :: HandheldPuzzle -> Instruction -> HandheldPuzzle
update puzz (NoOp _) =
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

solvePuzzleNoChange :: HandheldPuzzle -> HandheldPuzzle
solvePuzzleNoChange puzz =
  let instructionM = instructionSet puzz !? nextLine puzz
      newPuzzM = update puzz <$> instructionM
   in case newPuzzM of
        Just newPuzz ->
          if (nextLine newPuzz `elem` executedLines newPuzz)
            then newPuzz {haltCause = Just LoopDetected}
            else solvePuzzleNoChange newPuzz
        Nothing ->
          puzz {haltCause = Just Termination}

solvePuzzle :: HandheldPuzzle -> HandheldPuzzle
solvePuzzle puzz =
  let instructionM = instructionSet puzz !? nextLine puzz
      newPuzzM =
        ( \instruction -> case instruction of
            Jump x -> solvePuzzleNoChange $ update puzz (NoOp x)
            NoOp x -> solvePuzzleNoChange $ update puzz (Jump x)
            inst -> solvePuzzle $ update puzz inst
        )
          <$> instructionM
      result = liftA2 (,) instructionM newPuzzM
   in case result of
        Just (_, p@(HandheldPuzzle _ _ _ _ (Just Termination))) -> p
        Just (inst, (HandheldPuzzle _ _ _ _ (Just LoopDetected))) ->
          solvePuzzle $ update puzz inst
        _ -> puzz

puzzleParser :: Parser HandheldPuzzle
puzzleParser =
  (\instructions -> HandheldPuzzle Set.empty (V.fromList instructions) 0 0 Nothing)
    <$> sepBy instructionParser eol

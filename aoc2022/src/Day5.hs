{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Monad (replicateM_)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, mapMaybe)
import Lib.Stack (Stack, ofAs, pop, popSeveral, push, top)
import Parser (Parser)
import Text.Megaparsec (sepBy, sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol, spaceChar, string, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)

newtype Puzzle = Puzzle [Stack Char] deriving (Eq)

data Instruction = Instruction
  { howMany :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Eq, Show)

presentEntryParser :: Parser (Maybe Char)
presentEntryParser = Just <$> (char '[' *> upperChar <* char ']')

absentEntryParser :: Parser (Maybe Char)
absentEntryParser = Nothing <$ string "   "

rowParser :: Parser [Maybe Char]
rowParser = (presentEntryParser <|> absentEntryParser) `sepBy` char ' '

-- embarrassing, but I'm not sure why I keep getting an empty list
-- at the end of this parse
puzzleRows :: Parser [[Maybe Char]]
puzzleRows = filter (not . null) <$> rowParser `sepBy` eol

footerParser :: Parser [Int]
footerParser =
  (spaceChar *> decimal <* spaceChar) `sepBy` char ' '

idxToStack :: Int -> [[Maybe Char]] -> IO (Stack Char)
idxToStack idx rows =
  let entries = reverse (mapMaybe (!! (idx - 1)) rows)
   in ofAs entries

rowsToPuzzle :: [Int] -> [[Maybe Char]] -> IO Puzzle
rowsToPuzzle indices rows = Puzzle <$> traverse (`idxToStack` rows) indices

puzzleParser :: Parser (IO Puzzle)
puzzleParser = do
  rows <- puzzleRows
  indices <- footerParser
  pure $ rowsToPuzzle indices rows

instructionParser :: Parser Instruction
instructionParser = do
  _ <- string "move "
  howMany <- decimal
  _ <- string " from "
  from <- decimal
  _ <- string " to "
  to <- decimal
  pure $ Instruction {howMany, from, to}

fullParser :: Parser (IO Puzzle, [Instruction])
fullParser = do
  puzzIO <- puzzleParser <* eol <* eol
  instructions <- instructionParser `sepEndBy` eol
  pure (puzzIO, instructions)

step1 :: Puzzle -> Instruction -> IO ()
step1 (Puzzle stacks) (Instruction {howMany, from, to}) =
  replicateM_
    howMany
    ( do
        fromValue <- pop $ stacks !! (from - 1)
        traverse_ (\v -> push v (stacks !! (to - 1))) fromValue
    )

step2 :: Puzzle -> Instruction -> IO ()
step2 (Puzzle stacks) (Instruction {howMany, from, to}) =
  do
    fromValues <- popSeveral howMany $ stacks !! (from - 1)
    traverse_ (\v -> push v (stacks !! (to - 1))) fromValues

solvePart1 :: Puzzle -> [Instruction] -> IO String
solvePart1 puzzle@(Puzzle stacks) instructions =
  traverse_ (step1 puzzle) instructions >> (catMaybes <$> traverse top stacks)

solvePart2 :: Puzzle -> [Instruction] -> IO String
solvePart2 puzzle@(Puzzle stacks) instructions =
  traverse_ (step2 puzzle) instructions >> (catMaybes <$> traverse top stacks)

module Day4 where

import AoC.Parser (Parser)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

newtype Bookend = Bookend Int deriving (Eq, Ord, Show)

data ElfAssignment = ElfAssignment
  { startSection :: Bookend,
    endSection :: Bookend
  }
  deriving (Eq, Show)

data ElfPair = ElfPair ElfAssignment ElfAssignment deriving (Eq, Show)

fullCovers :: ElfAssignment -> ElfAssignment -> Bool
fullCovers
  (ElfAssignment {startSection = s1, endSection = e1})
  (ElfAssignment {startSection = s2, endSection = e2}) =
    -- range 1 includes the start of range 2
    s1 <= s2 && e1 >= e2

partialCovers :: ElfAssignment -> ElfAssignment -> Bool
partialCovers
  (ElfAssignment {startSection = s1, endSection = e1})
  (ElfAssignment {startSection = s2, endSection = e2}) =
    s1 <= s2 && e1 >= s2
      -- range 1 includes the end of range 2
      || s1 <= e2 && e1 >= e2

fullCovered :: ElfPair -> Bool
fullCovered (ElfPair assignment1 assignment2) =
  assignment1
    `fullCovers` assignment2
    || assignment2
    `fullCovers` assignment1

partiallyCovered :: ElfPair -> Bool
partiallyCovered (ElfPair assignment1 assignment2) =
  assignment1 `partialCovers` assignment2
  || assignment2 `partialCovers` assignment1

elfAssignmentParser :: Parser ElfAssignment
elfAssignmentParser = do
  start <- Bookend <$> decimal <* char '-'
  end <- Bookend <$> decimal
  pure $ ElfAssignment start end

elfPairParser :: Parser ElfPair
elfPairParser = do
  assignment1 <- elfAssignmentParser <* char ','
  ElfPair assignment1 <$> elfAssignmentParser

puzzleParser :: Parser [ElfPair]
puzzleParser = elfPairParser `sepEndBy` eol

solvePart1 :: [ElfPair] -> Int
solvePart1 = length . filter fullCovered

solvePart2 :: [ElfPair] -> Int
solvePart2 = length . filter partiallyCovered

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day3 where

import AoC.Parser (Parser)
import Data.Char (isLower, ord)
import Data.Monoid (Sum (..))
import Data.Set (Set, fromList, intersection, union)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (eol, letterChar)
import Prelude hiding (subtract)

newtype Priority = Priority (Sum Int)
  deriving (Eq, Show)
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

newtype ElfTeam = ElfTeam (Rucksack, Rucksack, Rucksack) deriving (Eq, Show)

priority :: Char -> Priority
priority c =
  let subtract = if isLower c then 96 else 38
   in Priority . Sum $ ord c - subtract

data Rucksack = Rucksack
  { compartmentA :: [Char],
    compartmentB :: [Char]
  }
  deriving (Eq, Show)

rucksackParser :: Parser Rucksack
rucksackParser =
  let fromChars :: [Char] -> Rucksack
      fromChars cs =
        let (firstHalf, secondHalf) = splitAt (length cs `div` 2) cs
         in Rucksack firstHalf secondHalf
   in fromChars <$> some letterChar

elfTeamParser :: Parser ElfTeam
elfTeamParser = do
  r1 <- rucksackParser <* eol
  r2 <- rucksackParser <* eol
  r3 <- rucksackParser
  pure $ ElfTeam (r1, r2, r3)

puzzleParser :: Parser [Rucksack]
puzzleParser = rucksackParser `sepEndBy` eol

puzzleParser2 :: Parser [ElfTeam]
puzzleParser2 = elfTeamParser `sepEndBy` eol

duplicatedChars :: Rucksack -> Set Char
duplicatedChars (Rucksack {compartmentA, compartmentB}) =
  let setA = fromList compartmentA
      setB = fromList compartmentB
   in setA `intersection` setB

priorityScore :: Set Char -> Priority
priorityScore = foldMap priority

scoreRucksack :: Rucksack -> Priority
scoreRucksack = priorityScore . duplicatedChars

solvePart1 :: [Rucksack] -> Int
solvePart1 rucksacks =
  let (Priority x) = foldMap scoreRucksack rucksacks
   in getSum x

scoreElfTeam :: ElfTeam -> Priority
scoreElfTeam
  ( ElfTeam
      ( Rucksack {compartmentA = a1, compartmentB = b1},
        Rucksack {compartmentA = a2, compartmentB = b2},
        Rucksack {compartmentA = a3, compartmentB = b3}
        )
    ) =
    let allElf1 = fromList a1 `union` fromList b1
        allElf2 = fromList a2 `union` fromList b2
        allElf3 = fromList a3 `union` fromList b3
     in priorityScore $ allElf1 `intersection` allElf2 `intersection` allElf3

solvePart2 :: [ElfTeam] -> Int
solvePart2 teams =
  let (Priority x) = foldMap scoreElfTeam teams
   in getSum x

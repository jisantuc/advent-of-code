{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many)
import Text.Megaparsec.Char (char, eol, letterChar)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)

data Policy = Policy
  { character :: Char,
    minTimes :: Int,
    maxTimes :: Int
  }
  deriving (Show)

type PuzzleLine = (Policy, String)

type Parser = Parsec Void Text

isValid :: PuzzleLine -> Bool
isValid ((Policy c min max), pw) =
  let charCount =
        foldl
          (\s newChar -> if (newChar == c) then (s + 1) else s)
          0
          pw
   in charCount >= min && charCount <= max

puzzleLineParser :: Parser PuzzleLine
puzzleLineParser = do
  min <- decimal
  char '-'
  max <- decimal
  char ' '
  policyCharacter <- charLiteral
  char ':' *> char ' '
  pw <- many letterChar
  eol
  pure $ (Policy policyCharacter min max, pw)

puzzleParser :: Parser [PuzzleLine]
puzzleParser = many puzzleLineParser

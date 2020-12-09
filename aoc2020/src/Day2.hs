{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many)
import Text.Megaparsec.Char (char, eol, letterChar)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)

data Policy = Policy
  { character :: Char,
    position1 :: Int,
    position2 :: Int
  }
  deriving (Show)

type PuzzleLine = (Policy, String)

type Parser = Parsec Void Text

xor :: Bool -> Bool -> Bool
xor = (/=)

isValid :: PuzzleLine -> Bool
isValid ((Policy c pos1 pos2), pw) =
  let charAt1 = pw !! (pos1 - 1)
      charAt2 = pw !! (pos2 - 1)
   in (charAt1 == c) `xor` (charAt2 == c)

puzzleLineParser :: Parser PuzzleLine
puzzleLineParser = do
  pos1 <- decimal
  char '-'
  pos2 <- decimal
  char ' '
  policyCharacter <- charLiteral
  char ':' *> char ' '
  pw <- many letterChar
  eol
  pure $ (Policy policyCharacter pos1 pos2, pw)

puzzleParser :: Parser [PuzzleLine]
puzzleParser = many puzzleLineParser

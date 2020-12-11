module Day6 where

import Data.List (foldl')
import qualified Data.Set as Set
import Parser (Parser)
import Text.Megaparsec (sepBy1, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, eol)

type AnswerGroup = Set.Set Char

answerGroupParser :: Parser AnswerGroup
answerGroupParser =
  let allLetters = Set.fromList ['a' .. 'z']
   in foldl' (\s1 s2 -> Set.intersection s1 s2) allLetters <$> sepEndBy (Set.fromList <$> some alphaNumChar) eol

puzzleParser :: Parser [AnswerGroup]
puzzleParser = sepBy1 answerGroupParser eol

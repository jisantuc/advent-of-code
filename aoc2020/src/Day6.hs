module Day6 where

import qualified Data.Set as Set
import Parser (Parser)
import Text.Megaparsec (sepBy1, sepEndBy, some)
import Text.Megaparsec.Char (alphaNumChar, eol)

type AnswerGroup = Set.Set Char

answerGroupParser :: Parser AnswerGroup
answerGroupParser =
  mconcat <$> sepEndBy (Set.fromList <$> some alphaNumChar) eol

puzzleParser :: Parser [AnswerGroup]
puzzleParser = sepBy1 answerGroupParser eol

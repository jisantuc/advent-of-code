{-# LANGUAGE NamedFieldPuns #-}

module Day2 where

import Control.Applicative ((<|>))
import Parser (Parser)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char, eol, space)

data Throw
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

data TargetResult
  = Win
  | Lose
  | Draw

data Round = Round
  { opponentThrow :: Throw,
    response :: Throw
  }
  deriving (Eq, Show)

data Round2 = Round2
  { opponentThrow' :: Throw
  , targetResult :: TargetResult }

parseTargetResult :: Parser TargetResult
parseTargetResult =
  Lose <$ char 'X'
  <|> Draw <$ char 'Y'
  <|> Win <$ char 'Z'

parseThrow :: Parser Throw
parseThrow =
  Rock <$ (char 'A' <|> char 'X')
    <|> Paper <$ (char 'B' <|> char 'Y')
    <|> Scissors <$ (char 'C' <|> char 'Z')

parseRound :: Parser Round
parseRound =
  Round <$> parseThrow <*> (space *> parseThrow)

parseRound2 :: Parser Round2
parseRound2 =
  Round2 <$> parseThrow <*> (space *> parseTargetResult)

puzzleParser1 :: Parser [Round]
puzzleParser1 = parseRound `sepEndBy` eol

puzzleParser2 :: Parser[Round2]
puzzleParser2 = parseRound2 `sepEndBy` eol

scoreThrow :: Throw -> Integer
scoreThrow Rock = 1
scoreThrow Paper = 2
scoreThrow Scissors = 3

scoreResult1 :: Round -> Integer
scoreResult1 (Round {opponentThrow, response}) =
  case (opponentThrow, response) of
    (Rock, Paper) -> 6
    (Rock, Scissors) -> 0
    (Paper, Rock) -> 0
    (Paper, Scissors) -> 6
    (Scissors, Rock) -> 6
    (Scissors, Paper) -> 0
    _ -> 3

throwFor :: Throw -> TargetResult -> Throw
throwFor t Draw = t
throwFor Rock Win = Paper
throwFor Rock Lose = Scissors
throwFor Paper Win = Scissors
throwFor Paper Lose = Rock
throwFor Scissors Win = Rock
throwFor Scissors Lose = Paper

scoreResult2 :: Round2 -> Integer
scoreResult2 (Round2 {opponentThrow', targetResult}) =
  case (opponentThrow', targetResult) of
    (t, Draw) -> scoreThrow t + 3
    (t, Win) -> scoreThrow (throwFor t Win) + 6
    (t, Lose) -> scoreThrow (throwFor t Lose)

solvePart1 :: [Round] -> Integer
solvePart1 [] = 0
solvePart1 (round@(Round {response}) : xs) =
  let throwScore = scoreThrow response
      roundScore = scoreResult1 round
      total = throwScore + roundScore
  in total + solvePart1 xs

solvePart2 :: [Round2] -> Integer
solvePart2 = foldr ((+) . scoreResult2) 0

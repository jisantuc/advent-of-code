{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import AoC.Parser (Parser)
import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.Monoid (Sum (..))
import Text.Megaparsec (sepBy, sepEndBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

data PuzzleLine = PuzzleLine
  { answer :: Int,
    operands :: [Int]
  }
  deriving (Eq, Show)

data Operator = Add | Multiply | Concatenate deriving (Eq, Show)

puzzleParser :: Parser [PuzzleLine]
puzzleParser =
  let lineParser = do
        answer <- decimal
        void ": "
        operands <- decimal `sepBy` " "
        pure $ PuzzleLine {answer, operands}
   in lineParser `sepEndBy` eol

findEquation :: PuzzleLine -> Maybe [Operator]
findEquation (PuzzleLine {answer, operands}) =
  go [] operands
  where
    go _ [] = Nothing
    go acc [x]
      | x == answer = Just acc
      | otherwise = Nothing
    go acc [x, y]
      | x + y == answer = Just (acc <> [Add])
      | x * y == answer = Just (acc <> [Multiply])
      | otherwise = Nothing
    go acc (x : y : t) =
      go (acc <> [Add]) (x + y : t)
        <|> go (acc <> [Multiply]) (x * y : t)

findEquation2 :: PuzzleLine -> Maybe [Operator]
findEquation2 (PuzzleLine {answer, operands}) =
  go [] operands
  where
    conc :: Int -> Int -> Int
    conc x y = read (show x <> show y)

    go _ [] = Nothing
    go acc [x]
      | x == answer = Just acc
      | otherwise = Nothing
    go acc [x, y]
      | x + y == answer = Just (acc <> [Add])
      | x * y == answer = Just (acc <> [Multiply])
      | conc x y == answer = Just (acc <> [Concatenate])
      | otherwise = Nothing
    go acc (x : y : t)
      | x >= answer = Nothing
      | otherwise =
          go (acc <> [Add]) (x + y : t)
            <|> go (acc <> [Multiply]) (x * y : t)
            <|> go (acc <> [Concatenate]) (conc x y : t)

solve :: (PuzzleLine -> Maybe [Operator]) -> [PuzzleLine] -> Int
solve f puzzleLines =
  let validLines = filter (isJust . f) puzzleLines
      answers = Sum . answer <$> validLines
   in getSum . mconcat $ answers

solve1 :: [PuzzleLine] -> Int
solve1 = solve findEquation

solve2 :: [PuzzleLine] -> Int
solve2 = solve findEquation2

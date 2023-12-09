{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (many, sepEndBy)
import Text.Megaparsec.Char (alphaNumChar, eol)

data Instruction = GoRight | GoLeft deriving (Eq, Show)

instructionParser :: Parser Instruction
instructionParser =
  Parser.fromMap $
    Map.fromList [("R", GoRight), ("L", GoLeft)]

data Node = Node {left :: Text, right :: Text} deriving (Eq, Show)

nodeLineParser :: Parser (Map.Map Text Node)
nodeLineParser = do
  label <- Text.pack <$> many alphaNumChar <* " = ("
  left <- Text.pack <$> many alphaNumChar <* ", "
  right <- Text.pack <$> many alphaNumChar <* ")"
  pure $ Map.singleton label (Node {left, right})

data Puzzle = Puzzle {steps :: [Instruction], nodes :: Map.Map Text Node} deriving (Eq, Show)

parser :: Parser Puzzle
parser = do
  steps <- many instructionParser <* eol <* eol
  nodes <- mconcat <$> nodeLineParser `sepEndBy` eol
  pure $ Puzzle {steps, nodes}

solver1 :: Puzzle -> Text -> Int -> Int
solver1 puzz@(Puzzle {steps, nodes}) startNode startCount =
  case foldl'
    ( \(label, count) step ->
        let options = Map.lookup label nodes
         in case (step, options) of
              (_, Nothing) -> (label, count)
              (GoRight, Just (Node _ right)) -> (right, count + 1)
              (GoLeft, Just (Node left _)) -> (left, count + 1)
    )
    (startNode, startCount)
    steps of
    ("ZZZ", n) -> n
    (lb, n) -> solver1 puzz lb n

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Foldable (Foldable (foldMap'), foldl')
import qualified Data.Map as Map
import Data.Monoid (Product (..))
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

data Puzzle = Puzzle
  { steps :: [Instruction],
    nodes :: Map.Map Text Node
  }
  deriving (Eq, Show)

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

stepper :: Map.Map Text Node -> Instruction -> Text -> Text
stepper m instruction label =
  maybe
    ""
    ( case instruction of
        GoRight -> right
        GoLeft -> left
    )
    (Map.lookup label m)

endsWith :: Char -> Text -> Bool
endsWith c t = case Text.unsnoc t of
  Just (_, c') | c == c' -> True
  _ -> False

getStartingNodes :: Puzzle -> [Text]
getStartingNodes = filter (endsWith 'A') . Map.keys . nodes

findZs :: Map.Map Text Node -> [Instruction] -> Text -> Int -> Int -> [Int]
findZs m instructions startLabel startCount searchSize =
  let endsWithZ = endsWith 'Z'
      (endingZs, endingLabel, endingCount) =
        foldl
          ( \acc@(zs, currLabel, count) instr ->
              case (Map.lookup currLabel m, count, instr) of
                (Just (Node {left}), ct, GoLeft) ->
                  if endsWithZ left
                    then (zs ++ [ct + 1], left, ct + 1)
                    else (zs, left, ct + 1)
                (Just (Node {right}), ct, GoRight) ->
                  if endsWithZ right
                    then (zs ++ [ct + 1], right, ct + 1)
                    else (zs, right, ct + 1)
                (Nothing, _, _) -> acc
          )
          ([], startLabel, startCount)
          instructions
   in if endingCount >= searchSize
        then endingZs
        else
          endingZs
            ++ findZs
              m
              instructions
              endingLabel
              (length instructions + startCount)
              searchSize

solver2 :: Puzzle -> [Text] -> Integer
solver2 (Puzzle {nodes, steps}) startNodes =
  let endingPositionLists =
        ( \label ->
            findZs nodes steps label 0 1000000
        )
          <$> startNodes
      evenlyDivides m n = snd (divMod m n) == 0
      stepLength = length steps
      firstZs = head <$> endingPositionLists
      factors =
        ( \z ->
            if evenlyDivides z stepLength then z `div` stepLength else z
        )
          <$> firstZs
   in getProduct $ foldMap' Product (fromIntegral <$> stepLength : factors)

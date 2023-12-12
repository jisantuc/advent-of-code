{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Debug.Trace (traceShow)
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

findZs :: Map.Map Text Node -> [Instruction] -> Text -> Integer -> Integer -> [Integer] -> (Text, [Integer])
findZs m instructions startLabel searchSize startCount startPositions =
  -- until count gets to
  let endsWithZ = endsWith 'Z'
      (endingZs, endingLabel, endingCount) =
        foldl'
          ( \acc@(zs, currLabel, count) instr ->
              case (Map.lookup currLabel m, count, instr) of
                (_, ct, _) | ct == searchSize -> acc
                (Nothing, _, _) -> acc
                (Just (Node {left}), ct, GoLeft) ->
                  if endsWithZ left
                    then (zs ++ [ct + 1], left, ct + 1)
                    else (zs, left, ct + 1)
                (Just (Node {right}), ct, GoRight) ->
                  if endsWithZ right
                    then (zs ++ [ct + 1], right, ct + 1)
                    else (zs, right, ct + 1)
          )
          (startPositions, startLabel, startCount)
          instructions
   in if endingCount >= searchSize
        then (endingLabel, endingZs)
        else findZs m instructions endingLabel searchSize endingCount endingZs

solver2 :: Puzzle -> [Text] -> Integer -> Integer -> Integer -> Integer
solver2 puzz@(Puzzle {nodes, steps}) startNodes startCount limit searchMultiplier =
  if startCount >= limit
    then limit
    else
      let searchSize = searchMultiplier * fromIntegral (length steps)
          (endingLabels, endingPositionLists) = unzip $ (\label -> findZs nodes steps label searchSize startCount []) <$> startNodes
          sharedZs =
            fromMaybe Set.empty $
              foldl'
                ( \acc s ->
                    Set.intersection s <$> acc <|> Just s
                )
                Nothing
                ( traceShow
                    ( "Start count: "
                        <> show startCount
                        <> " search size: "
                        <> show searchSize
                        <> " ending position lists: "
                        <> show endingPositionLists
                        <> "ending labels: "
                        <> show endingLabels
                    )
                    (Set.fromList <$> endingPositionLists)
                )
       in if not $ Set.null sharedZs then minimum sharedZs else solver2 puzz endingLabels (searchSize + startCount) limit searchMultiplier

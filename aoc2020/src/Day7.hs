{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import Parser (Parser)
import Text.Megaparsec (MonadParsec (try), manyTill, sepBy, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, string)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)

newtype BagColor = BagColor String deriving (Eq, Ord, Show)

type BagPolicy = M.Map BagColor (Set.Set Rule)

data Rule = Rule
  { requiredBagColor :: BagColor,
    howMany :: Integer
  }
  deriving (Eq, Ord, Show)

singleton :: BagColor -> [Rule] -> BagPolicy
singleton color rules = M.singleton color (Set.fromList rules)

bagParser :: Parser Text
bagParser =
  try $ string " bags" <|> string " bag"

bagColorParser :: Parser BagColor
bagColorParser =
  BagColor . unwords <$> sepBy (some alphaNumChar) (char ' ')

ruleParser :: Parser Rule
ruleParser = do
  howMany <- decimal
  char ' '
  color <- BagColor <$> manyTill charLiteral bagParser
  pure $ Rule color howMany

bagContainedParser :: Parser [Rule]
bagContainedParser =
  ([] <$ string "no other bags")
    <|> (sepBy ruleParser (string ", "))

policyLineParser :: Parser BagPolicy
policyLineParser = do
  container <- BagColor <$> manyTill charLiteral (string " bags contain ")
  rules <- bagContainedParser
  char '.'
  pure $ singleton container rules

puzzleParser :: Parser BagPolicy
puzzleParser =
  List.foldl' (M.unionWith (<>)) M.empty <$> sepBy policyLineParser eol

shinyGold :: BagColor
shinyGold = BagColor "shiny gold"

countContainedBags :: BagPolicy -> BagColor -> Integer
countContainedBags policy color =
  let rules = M.lookup color policy
   in case rules of
        Just bagRules ->
          sum $
            (\(Rule bc hm) -> hm * countContainedBags policy bc + hm)
              <$> Set.toList bagRules
        Nothing -> 0

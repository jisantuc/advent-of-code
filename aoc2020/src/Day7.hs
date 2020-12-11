{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Foldable (Foldable (fold))
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Parser (Parser)
import Text.Megaparsec (MonadParsec (try), manyTill, sepBy, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, string)
import Text.Megaparsec.Char.Lexer (charLiteral, decimal)

newtype BagColor = BagColor String deriving (Eq, Ord, Show)

type BagPolicy = M.Map BagColor (Set.Set BagColor)

singleton :: BagColor -> BagColor -> BagPolicy
singleton color containedBy = M.singleton color (Set.singleton containedBy)

addRule :: BagPolicy -> BagColor -> BagColor -> BagPolicy
addRule policy color rule = policy <> singleton color rule

bagParser :: Parser Text
bagParser =
  try $ string " bags" <|> string " bag"

bagColorParser :: Parser BagColor
bagColorParser =
  BagColor . unwords <$> sepBy (some alphaNumChar) (char ' ')

bagContainedParser :: Parser [BagColor]
bagContainedParser =
  ([] <$ string "no other bags")
    <|> (sepBy (decimal *> char ' ' *> (BagColor <$> manyTill charLiteral bagParser)) (string ", "))

policyLineParser :: Parser BagPolicy
policyLineParser = do
  container <- BagColor <$> manyTill charLiteral (string " bags contain ")
  rules <- bagContainedParser
  char '.'
  let combined = singleton <$> rules <*> [container]
  pure $ mconcat combined

puzzleParser :: Parser BagPolicy
puzzleParser =
  List.foldl' (M.unionWith (<>)) M.empty <$> sepBy policyLineParser eol

shinyGold :: BagColor
shinyGold = BagColor "shiny gold"

findContainersFor :: BagColor -> BagPolicy -> Set.Set BagColor
findContainersFor color policy =
  let directContainers = fromMaybe Set.empty (M.lookup color policy)
      ancestralContainers = fold $ Set.map (\c -> findContainersFor c policy) directContainers
   in directContainers <> ancestralContainers

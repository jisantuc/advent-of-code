{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Foldable (foldl')
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (eol, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Instruction
  = Vertical Int
  | Horizontal Int
  deriving (Eq, Show)

data Location a = Location
  { x :: a,
    z :: a,
    aim :: a
  }
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Location a) where
  Location x1 z1 aim1 <> Location x2 z2 aim2 = Location (x1 <> x2) (z1 <> z2) (aim1 <> aim2)

instance Monoid a => Monoid (Location a) where
  mempty = Location mempty mempty mempty

type Puzzle = [Instruction]

parseInst :: (Int -> Instruction) -> Text -> Parser Instruction
parseInst f s = f <$> (string s *> space *> signed space decimal)

puzzleParser :: Parser Puzzle
puzzleParser =
  let instrParser =
        parseInst Horizontal "forward"
          <|> parseInst (Vertical . negate) "up"
          <|> parseInst Vertical "down"
   in instrParser `sepEndBy` eol

update :: Location (Sum Int) -> Instruction -> Location (Sum Int)
update (Location {x, z, aim}) (Horizontal n) =
  Location (x <> Sum n) z aim
update (Location {x, z, aim}) (Vertical n) =
  Location x (z <> Sum n) aim

solvePart1 :: Puzzle -> Int
solvePart1 puzz =
  (\(Location {x, z}) -> getSum x * getSum z) $ foldl' update mempty puzz

updatePart2 :: Location (Sum Int) -> Instruction -> Location (Sum Int)
updatePart2 (Location {x, z, aim}) (Horizontal n) =
  Location (x <> Sum n) (z + aim * Sum n) aim
updatePart2 (Location {x, z, aim}) (Vertical n) =
  Location x z (aim <> Sum n)

solvePart2 :: Puzzle -> Int
solvePart2 puzz = (\(Location {x, z}) -> getSum x * getSum z) $ foldl' updatePart2 mempty puzz

solve :: Puzzle -> Int
solve = solvePart2

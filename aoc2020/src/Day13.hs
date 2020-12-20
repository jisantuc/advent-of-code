module Day13 where

import Data.List
import Parser (Parser)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = Puzzle
  { timestamp :: Integer,
    busIds :: [Integer]
  }
  deriving (Eq, Show)

solve :: Puzzle -> Integer
solve (Puzzle ts ids) =
  let (busId, departureTime) =
        minimumBy (\(_, a) (_, b) -> compare a b) $
          (\busId -> (busId, ts - (ts `mod` busId) + busId)) <$> ids
   in busId * (departureTime - ts)

busIdParser :: Parser [Integer]
busIdParser = [] <$ char 'x' <|> (: []) <$> decimal

puzzleParser :: Parser Puzzle
puzzleParser = do
  ts <- decimal <* eol
  busIds <-
    mconcat <$> sepEndBy busIdParser (char ',')
  pure $ Puzzle ts busIds

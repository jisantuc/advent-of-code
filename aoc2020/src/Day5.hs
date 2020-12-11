module Day5 where

import Control.Monad (replicateM)
import qualified Data.Set as Set
import Parser (Parser)
import Text.Megaparsec (sepBy, (<|>))
import Text.Megaparsec.Char (char, eol)

data Seat = Seat
  { row :: Int,
    col :: Int
  }
  deriving (Show)

parseBit :: Char -> Char -> Parser Bool
parseBit zero one =
  False <$ char zero
    <|> True <$ char one

parserRowBit :: Parser Bool
parserRowBit = parseBit 'F' 'B'

parserColBit :: Parser Bool
parserColBit = parseBit 'L' 'R'

toInt :: [Bool] -> Int
toInt [] = 0
toInt bools =
  fromInteger . snd $
    foldr
      (\v (pow, acc) -> (pow + 1, acc + if (v) then 2 ^ pow else 0))
      (0, 0)
      bools

seatParser :: Parser Seat
seatParser = do
  rowBits <- replicateM 7 parserRowBit
  colBits <- replicateM 3 parserColBit
  let rowNum = toInt rowBits
  let colNum = toInt colBits
  pure $ Seat rowNum colNum

puzzleParser :: Parser [Seat]
puzzleParser = sepBy seatParser eol

seatId :: Seat -> Int
seatId (Seat r c) = 8 * r + c

findMissingSeat :: [Seat] -> Set.Set Int
findMissingSeat [] = Set.empty
findMissingSeat seats =
  let seatIds = Set.fromList $ seatId <$> seats
   in Set.filter (\x -> ((not $ x + 1 `elem` seatIds) && x + 2 `elem` seatIds)) seatIds

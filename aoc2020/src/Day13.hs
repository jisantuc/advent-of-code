module Day13 where

import Control.Monad (guard)
import Data.Foldable (maximumBy)
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Parser (Parser)
import Text.Megaparsec (sepEndBy, (<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Puzzle = Puzzle
  { busIds :: [(Integer, Integer)]
  }
  deriving (Eq, Show)

solve :: Puzzle -> Maybe Integer
solve (Puzzle idsAndOffsets) =
  let stepper = maximumBy (\x y -> compare (fst x) (fst y)) idsAndOffsets
      maxBusId = fst stepper
      maxBusOffset = snd stepper
      candidates = (maxBusId *) <$> [1 ..]
      folder ts acc cmp@(busId, offset) =
        if (cmp == stepper)
          then acc
          else acc && ((ts + (offset - maxBusOffset)) `mod` busId == 0)
   in listToMaybe $ do
        candidate <- candidates
        guard $ List.foldl' (folder candidate) True idsAndOffsets
        pure $ (candidate - maxBusOffset)

busIdParser :: Parser (Maybe Integer)
busIdParser = Nothing <$ char 'x' <|> Just <$> decimal

puzzleParser :: Parser Puzzle
puzzleParser =
  let folder acc (busId, idx) =
        case busId of
          Just n -> acc ++ [(n, idx)]
          Nothing -> acc
   in do
        decimal <* eol -- throw away first line
        busIds <-
          sepEndBy busIdParser (char ',')
        let indexed = foldl folder [] (zip busIds [0 ..])
        pure $ Puzzle indexed

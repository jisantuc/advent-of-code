module Day9 where

import qualified Data.Vector as V
import Day1 (findTarget)
import Parser (Parser)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

solve :: Int -> Int -> V.Vector Int -> Int
solve startIndex lookback nums =
  let x = V.unsafeIndex nums startIndex
      preamble = V.take lookback . V.drop (startIndex - lookback) $ nums
   in case findTarget (toInteger x) (toInteger <$> V.toList preamble) of
        Just _ -> solve (startIndex + 1) lookback nums
        Nothing -> x

puzzleParser :: Parser (V.Vector Int)
puzzleParser = V.fromList <$> (sepBy decimal eol)

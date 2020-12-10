{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Control.Monad (guard)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Parser (Parser)
import Text.Megaparsec (MonadParsec (eof), many, (<|>))
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

lineParser :: Parser Integer
lineParser = decimal <* eol

puzzleParser :: Parser [Integer]
puzzleParser = many lineParser <* eof

rotateArray :: Int -> [Integer] -> [Integer]
rotateArray n arr = drop n arr ++ take n arr

findTarget :: Integer -> [Integer] -> Maybe (Integer, Integer)
findTarget n arr =
  go n 1 arr
  where
    tupSum (x, y) = x + y
    go target rotation src =
      (find (\x -> tupSum x == target) $ zip src (rotateArray rotation src))
        <|> go target (rotation + 1) src

findTarget3 :: Integer -> [Integer] -> Maybe (Integer, Integer, Integer)
findTarget3 n arr =
  go n 1 2 arr
  where
    go target rot1 rot2 src
      | rot2 < length src - 1 =
        ( listToMaybe $ do
            x <- src
            y <- drop rot1 src
            z <- drop rot2 src
            guard $ (x + y + z) == target
            pure $ (x, y, z)
        )
          <|> go target rot1 (rot2 + 1) src
      | otherwise =
        ( listToMaybe $ do
            x <- src
            y <- drop rot1 src
            z <- drop rot2 src
            guard $ (x + y + z) == target
            pure $ (x, y, z)
        )
          <|> go target (rot1 + 1) (rot2 + 2) src

find2020 :: [Integer] -> Maybe (Integer, Integer)
find2020 = findTarget 2020

find32020 :: [Integer] -> Maybe (Integer, Integer, Integer)
find32020 = findTarget3 2020

tupProduct :: (Integer, Integer) -> Integer
tupProduct (x, y) = x * y

tupProduct3 :: (Integer, Integer, Integer) -> Integer
tupProduct3 (x, y, z) = x * y * z

testInput :: Text
testInput = pack $ unlines ["1721", "979", "366", "299", "675", "1456"]

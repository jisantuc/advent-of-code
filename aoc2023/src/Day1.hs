{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Functor (($>), (<&>))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Megaparsec (many, runParser, sepBy, (<|>))
import Text.Megaparsec.Byte (alphaNumChar, eol)

type Puzzle = [Int]

wordToDigit :: Map.Map [Char] Int
wordToDigit =
  Map.fromList
    [ ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    ]

spelledOutDigitParser :: Parser Int
spelledOutDigitParser =
  Parser.fromMap wordToDigit

reverseSpelledOutDigits :: Parser Int
reverseSpelledOutDigits =
  let reversedKeysMap = Map.mapKeys reverse wordToDigit
   in Parser.fromMap reversedKeysMap

plainDigitParser :: Parser Int
plainDigitParser =
  "1"
    $> 1 <|> "2"
    $> 2
      <|> "3"
    $> 3
      <|> "4"
    $> 4
      <|> "5"
    $> 5
      <|> "6"
    $> 6
      <|> "7"
    $> 7
      <|> "8"
    $> 8
      <|> "9"
    $> 9

day1Parser :: Parser [Int]
day1Parser =
  let lineParser = many alphaNumChar
   in ( lineParser
          <&> ( \line ->
                  let repacked = BS.pack line
                   in if BS.null repacked
                        then 0
                        else
                          let digits = (findFirstDigit repacked False : [findFirstDigit (BS.reverse repacked) True])
                           in read . mconcat $ show . fromMaybe 0 <$> digits
              )
      )
        `sepBy` eol

findFirstDigit :: ByteString -> Bool -> Maybe Int
findFirstDigit bytes doReverse =
  ( \case
      Right digit -> Just digit
      Left _ -> if BS.null bytes then Nothing else findFirstDigit (BS.tail bytes) doReverse
  )
    (runParser (plainDigitParser <|> if doReverse then reverseSpelledOutDigits else spelledOutDigitParser) "" bytes)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant $" #-}

module Day1 where

import AoC.Parser (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.Megaparsec (many, runParser, sepBy, (<|>))
import Text.Megaparsec.Byte (alphaNumChar, eol, string)

type Puzzle = [Int]

spelledOutDigitParser :: Parser Int
spelledOutDigitParser =
  string "one"
    $> 1
      <|> string "two"
    $> 2
      <|> string "three"
    $> 3
      <|> string "four"
    $> 4
      <|> string "five"
    $> 5
      <|> string "six"
    $> 6
      <|> string "seven"
    $> 7
      <|> string "eight"
    $> 8
      <|> string "nine"
    $> 9

reverseSpelledOutDigits :: Parser Int
reverseSpelledOutDigits =
  string "eno"
    $> 1
      <|> string "owt"
    $> 2
      <|> string "eerht"
    $> 3
      <|> string "ruof"
    $> 4
      <|> string "evif"
    $> 5
      <|> string "xis"
    $> 6
      <|> string "neves"
    $> 7
      <|> string "thgie"
    $> 8
      <|> string "enin"
    $> 9

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
                          trace ("line is: " <> show (w2c <$> line)) $
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
    $ (runParser (plainDigitParser <|> if doReverse then reverseSpelledOutDigits else spelledOutDigitParser) "" bytes)

{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.List (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import Parser (Parser)
import Text.Megaparsec (MonadParsec (try), choice, many, sepBy1, sepEndBy, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

data PassportField
  = BirthYear Integer
  | IssueYear Integer
  | ExpirationYear Integer
  | Height Integer (Maybe Unit)
  | HairColor String Bool
  | EyeColor String
  | PassportId String
  | CountryId Integer
  deriving (Eq, Ord, Show)

data Validation = Validation
  { containsBirth :: Bool,
    containsIssueYear :: Bool,
    containsExpiration :: Bool,
    containsHeight :: Bool,
    containsHair :: Bool,
    containsEye :: Bool,
    containsPid :: Bool
  }

emptyValidation :: Validation
emptyValidation = Validation False False False False False False False

isValid :: Validation -> Bool
isValid (Validation byr iyr eyr hgt hair eye pid) =
  byr && iyr && eyr && hgt && hair && eye && pid

validate :: [PassportField] -> Validation
validate fields =
  let validEyes = Set.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
   in foldl'
        ( \validation field ->
            case field of
              (BirthYear yr) -> validation {containsBirth = yr >= 1920 && yr <= 2002}
              (IssueYear yr) -> validation {containsIssueYear = yr >= 2010 && yr <= 2020}
              (ExpirationYear yr) -> validation {containsExpiration = yr >= 2020 && yr <= 2030}
              (Height value (Just Centimeter)) -> validation {containsHeight = value >= 150 && value <= 193}
              (Height value (Just Inch)) -> validation {containsHeight = value >= 59 && value <= 76}
              (Height _ _) -> validation
              (HairColor _ b) -> validation {containsHair = b}
              (EyeColor color) -> validation {containsEye = color `elem` validEyes}
              (PassportId pid) -> validation {containsPid = length pid == 9}
              (CountryId _) -> validation
        )
        emptyValidation
        fields

data Unit
  = Inch
  | Centimeter
  deriving (Eq, Ord, Show)

fieldParser :: (a -> PassportField) -> Text -> Parser a -> Parser PassportField
fieldParser f chars valueParser =
  f
    <$> ( do
            string chars
            char ':'
            valueParser
        )

birthYearParser :: Parser PassportField
birthYearParser = fieldParser BirthYear "byr" decimal

issueYearParser :: Parser PassportField
issueYearParser = fieldParser IssueYear "iyr" decimal

expirationYearParser :: Parser PassportField
expirationYearParser = fieldParser ExpirationYear "eyr" decimal

unitParser :: Parser (Maybe Unit)
unitParser =
  ( try $
      Just Centimeter <$ string "cm"
        <|> Just Inch <$ string "in"
  )
    <|> pure Nothing

heightParser :: Parser PassportField
heightParser = Height <$> (string "hgt" *> char ':' *> decimal) <*> unitParser

colorParser :: Parser String
colorParser =
  (try $ char '#' *> many alphaNumChar)
    <|> many
      alphaNumChar

colorParserHair :: Parser (String, Bool)
colorParserHair = do
  hasPound <-
    (True <$ try (char '#')) <|> pure False
  color <- many alphaNumChar
  pure (color, hasPound)

hairColorParser :: Parser PassportField
hairColorParser = fieldParser (uncurry HairColor) "hcl" colorParserHair

eyeColorParser :: Parser PassportField
eyeColorParser = fieldParser EyeColor "ecl" colorParser

-- why colorParser? because the data are all messed up, and I can't guarantee
-- that puzzle input doesn't include passport IDs that are actually colors.
-- if I have to unmix this I'm gonna be sad.
passportIdParser :: Parser PassportField
passportIdParser = fieldParser PassportId "pid" colorParser

countryIdParser :: Parser PassportField
countryIdParser = fieldParser CountryId "cid" decimal

passportFieldParser :: Parser PassportField
passportFieldParser =
  choice
    [ birthYearParser,
      issueYearParser,
      expirationYearParser,
      heightParser,
      hairColorParser,
      eyeColorParser,
      passportIdParser,
      countryIdParser
    ]

passportParser :: Parser [PassportField]
passportParser =
  sepEndBy
    passportFieldParser
    ( () <$ char ' ' <|> () <$ eol
    )

puzzleParser :: Parser [[PassportField]]
puzzleParser = sepBy1 passportParser eol

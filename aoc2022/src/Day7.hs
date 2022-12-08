{-# LANGUAGE OverloadedStrings #-}

module Day7 where 

import Data.Text (Text, pack)
import Parser (Parser)
import Text.Megaparsec (many, notFollowedBy, sepEndBy, some, (<|>), sepBy)
import Text.Megaparsec.Char (alphaNumChar, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Directory = RootDir | NamedDir Text | DotDot deriving (Eq, Show)

data File = File
  { fileName :: Text,
    fileSize :: Integer
  }
  deriving (Eq, Show)

data OutputLine
  = DirectoryLine Directory
  | FileOutputLine File
  deriving (Eq, Show)

data Instruction
  = Cd Directory
  | Ls [OutputLine]
  deriving (Eq, Show)

dirTargetParser :: Parser Directory
dirTargetParser =
  RootDir <$ "/"
    <|> DotDot <$ ".."
    <|> NamedDir . pack <$> many alphaNumChar

cdParser :: Parser Instruction
cdParser =
  Cd <$> ("cd " *> dirTargetParser)

dirLineParser :: Parser OutputLine
dirLineParser = DirectoryLine <$> ("dir " *> dirTargetParser)

fnameParser :: Parser Text
fnameParser =
  let alphaNumString = pack <$> some alphaNumChar
      extensionParser = "." *> alphaNumString
   in do
        basename <- alphaNumString
        ext <- extensionParser
        pure $ basename <> "." <> ext

fileLineParser :: Parser OutputLine
fileLineParser = do
  fsize <- decimal <* space
  fname <- fnameParser
  pure . FileOutputLine $ File fname fsize

outputLinesParser :: Parser [OutputLine]
outputLinesParser =
  (dirLineParser <|> fileLineParser) `sepBy` (eol <* notFollowedBy "$ ")

lsParser :: Parser Instruction
lsParser =
  Ls <$> ("ls" *> eol *> outputLinesParser)

instructionParser :: Parser Instruction
instructionParser =
  "$ " *> (cdParser <|> lsParser)

puzzleParser :: Parser [Instruction]
puzzleParser = instructionParser `sepEndBy` eol

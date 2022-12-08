{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Functor (void)
import Data.Text (Text, pack)
import Parser (Parser)
import Text.Megaparsec (eof, lookAhead, many, some, someTill, withRecovery, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug (dbg)

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
  Cd <$> ("cd " *> dirTargetParser) <* eol

dirLineParser :: Parser OutputLine
dirLineParser = DirectoryLine <$> ("dir " *> dirTargetParser)

fnameParser :: Parser Text
fnameParser =
  let alphaNumString = pack <$> some alphaNumChar
      extensionParser = "." *> alphaNumString
   in do
        basename <- alphaNumString
        ext <- withRecovery (const "") extensionParser
        pure $ basename <> "." <> ext

fileLineParser :: Parser OutputLine
fileLineParser = do
  fsize <- decimal <* space
  fname <- fnameParser
  pure . FileOutputLine $ File fname fsize

outputLinesParser :: Parser [OutputLine]
outputLinesParser =
  someTill
    ((dirLineParser <|> fileLineParser) <* eol)
    (void (lookAhead "$ ") <|> eof)

lsParser :: Parser Instruction
lsParser =
  dbg "LsParser" $ Ls <$> ("ls" *> eol *> outputLinesParser)

instructionParser :: Parser Instruction
instructionParser =
  "$ " *> (cdParser <|> lsParser)

puzzleParser :: Parser [Instruction]
puzzleParser = some instructionParser

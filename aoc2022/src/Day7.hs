{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import Lib.Stack (Stack (..))
import Parser (Parser)
import Text.Megaparsec (many, sepBy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)

data Directory = RootDir | NamedDir Text | DotDot deriving (Eq, Show)

data File = File
  { fileName :: Text,
    fileSize :: Integer
  } deriving (Eq, Show)

data OutputLine
  = DirectoryLine Directory
  | FileOutputLine File deriving (Eq, Show)

newtype DirTree = DirTree (Map Directory DirTree) deriving (Eq, Show)

type Workdir = Stack Directory

data Instruction
  = Cd Directory
  | Ls [OutputLine] deriving (Eq, Show)

dirTargetParser :: Parser Directory
dirTargetParser =
  RootDir <$ "/"
    <|> DotDot <$ ".."
    <|> NamedDir . pack <$> many alphaNumChar

cdParser :: Parser Instruction
cdParser =
  Cd <$> ("$ cd " *> dirTargetParser)

outputLinesParser :: Parser [OutputLine]
outputLinesParser =
  let lineParser =
        DirectoryLine <$> ("dir " *> dirTargetParser)
          <|> do
            fSize <- decimal
            space
            fName <-
              pack <$> many alphaNumChar
                <* try ("." <* many alphaNumChar)
            pure . FileOutputLine $ File fName fSize
   in lineParser `sepBy` eol

lsParser :: Parser Instruction
lsParser =
  Ls <$> ("$ ls" *> eol *> outputLinesParser)

instructionParser :: Parser Instruction
instructionParser =
  cdParser <|> lsParser

puzzleParser :: Parser [Instruction]
puzzleParser = instructionParser `sepBy` eol

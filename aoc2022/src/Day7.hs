{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Functor (void, ($>), (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Lib.Stack (Stack, ofAs, pop)
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

data FileTree = FileTree
  { directory :: Directory,
    files :: [File],
    subTree :: Map Text FileTree
  }

data PuzzleState = PuzzleState
  { tree :: FileTree,
    workingDirectory :: Stack Directory
    -- maybe I need to track a _view_ instead of a stack of directories?
    -- i.e., given some ls output, I need to know how to update tree
    -- because FileTree is recursive, I should be able to keep stacking Map
    -- prisms on it for further nested updates
    -- in that case I no longer need to track workdir, but a FileTree to FileTree
    -- prism
    -- ".." handling is maybe harder, so maybe instead I need to be able to convert from
    -- a Stack to such an optic
  }

emptyFileTree :: FileTree
emptyFileTree = FileTree RootDir [] Map.empty

initialState :: IO PuzzleState
initialState =
  let initialWorkingDirectory = ofAs [RootDir]
   in PuzzleState emptyFileTree <$> initialWorkingDirectory

step :: PuzzleState -> Instruction -> IO PuzzleState
step state (Cd RootDir) = ofAs [RootDir] <&> (\x -> state {workingDirectory = x})
step _ (Cd (NamedDir _)) = undefined
step state@(PuzzleState {workingDirectory}) (Cd DotDot) =
  pop workingDirectory $> state
step _ _ = undefined
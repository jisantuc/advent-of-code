{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Foldable (foldlM)
import Data.Functor (void, ($>), (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Lib.Stack (Stack, key, ofAs, pop, push, top)
import Parser (Parser)
import Text.Megaparsec (eof, lookAhead, many, some, someTill, withRecovery, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug (dbg)

data Directory = RootDir | NamedDir Text | DotDot deriving (Eq, Show, Ord)

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
    subTree :: Set.Set Text
  }
  deriving (Eq, Show)

data PuzzleState = PuzzleState
  { trees :: Map Text FileTree,
    workingDirectory :: Stack Directory,
    directoryContents :: Map Text [OutputLine],
    spaceMap :: Map Directory Int
  }

instance Eq PuzzleState where
  (PuzzleState tree1 _ contents1 spaceMap1) == (PuzzleState tree2 _ contents2 spaceMap2) =
    tree1 == tree2 && contents1 == contents2 && spaceMap1 == spaceMap2

instance Show PuzzleState where
  show (PuzzleState {trees, directoryContents, spaceMap}) =
    "Tree "
      <> show trees
      <> ";\n Contents: "
      <> show directoryContents
      <> ";\n Space map: "
      <> show spaceMap

emptyFileTree :: FileTree
emptyFileTree = FileTree RootDir [] Set.empty

initialState :: IO PuzzleState
initialState =
  let initialWorkingDirectory = ofAs [RootDir]
   in PuzzleState Map.empty <$> initialWorkingDirectory <*> mempty <*> mempty

step :: PuzzleState -> Instruction -> IO PuzzleState
step state (Cd RootDir) = ofAs [RootDir] <&> (\x -> state {workingDirectory = x})
step state (Cd namedDir@(NamedDir _)) =
  let workdir = workingDirectory state
   in push namedDir workdir $> state
step state@(PuzzleState {workingDirectory}) (Cd DotDot) =
  pop workingDirectory $> state
step state@(PuzzleState {workingDirectory, directoryContents = initialContents, trees = initialTrees}) (Ls output) =
  do
    -- this is fine, because we always at least have a RootDir,
    -- it just _looks_ horrifying
    Just cwd <- top workingDirectory
    normPath <- pack <$> key workingDirectory "/"
    let prefix = normPath <> "/"
    let contentsForDir = Map.singleton normPath output
    pure $
      state
        { directoryContents = initialContents <> contentsForDir,
          trees =
            initialTrees <> Map.singleton normPath (FileTree { files =
                  ( \case
                      FileOutputLine f -> [f]
                      _ -> []
                  )
                    =<< output,
                subTree =
                  ( \case
                      DirectoryLine (NamedDir name) -> Set.singleton $ prefix <> name
                      _ -> Set.empty
                  )
                    `foldMap` output,
                directory = cwd
              })
        }

buildState :: [Instruction] -> IO PuzzleState
buildState instructions =
  initialState >>= \state -> foldlM step state instructions

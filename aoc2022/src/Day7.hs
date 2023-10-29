{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import Data.Foldable (foldl', foldlM)
import Data.Functor (void, ($>), (<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Lib.Stack (Stack, key, ofAs, pop, push, top)
import Parser (Parser)
import Text.Megaparsec (eof, lookAhead, many, some, someTill, withRecovery, (<|>))
import Text.Megaparsec.Char (alphaNumChar, eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug (dbg)

data Directory = RootDir | NamedDir Text | DotDot deriving (Eq, Ord)

instance Show Directory where
  show RootDir = "RootDir"
  show (NamedDir t) = unpack t
  show DotDot = ".."

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

_sizeOfNode :: Map Text FileTree -> Text -> Integer
_sizeOfNode nodes k =
  case Map.lookup k nodes of
    Just (FileTree {files, subTree}) ->
      let nodeFileSize = foldl' (+) 0 $ fileSize <$> files
          subTreeSizes = _sizeOfNode nodes <$> Set.toList subTree
       in sum (nodeFileSize : subTreeSizes)
    Nothing -> 0

treeSizes :: Map Text FileTree -> Text -> Map Text Integer
treeSizes nodes k =
  case Map.lookup k nodes of
    Just (FileTree {files, subTree}) ->
      let nodeFileSize = foldl' (+) 0 $ fileSize <$> files
          subTreeSizes = _sizeOfNode nodes <$> Set.toList subTree
       in Map.singleton k $ sum (nodeFileSize : subTreeSizes)
    Nothing -> Map.empty

fileTreeSizes :: Map Text FileTree -> Map Text Integer
fileTreeSizes m = Map.unions $ treeSizes m <$> Map.keys m

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
            initialTrees
              <> Map.singleton
                normPath
                ( FileTree
                    { files =
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
                    }
                )
        }

buildState :: [Instruction] -> IO PuzzleState
buildState instructions =
  initialState >>= \state -> foldlM step state instructions

-- for one node:
--   - add up its own file size
--   - for each of its children
--   - check if the child's key is in the cache
--   - otherwise, pass the child the cache, and calculate the child's size
--   - add up the children's sizes with the owned file size
calculateNodeSize ::
  IORef (Map Text Integer) ->
  Map Text FileTree ->
  Text ->
  IO Integer
calculateNodeSize directorySizeCache directories nodeKey =
  case Map.lookup nodeKey directories of
    Just (FileTree {subTree, files}) ->
      do
        cache <- readIORef directorySizeCache
        case Map.lookup nodeKey cache of
          Just answer -> pure answer
          Nothing ->
            do
              let ownDirectorySize = foldMap (Sum . fileSize) files
              childrenSizes <- traverse (fmap Sum . calculateNodeSize directorySizeCache directories) (Set.toList subTree)
              let out = getSum . mconcat $ ownDirectorySize : childrenSizes
              modifyIORef' directorySizeCache (Map.union (Map.singleton nodeKey out))
              pure out
    Nothing ->
      pure 0

buildSpaceMap :: PuzzleState -> IO (Map Text Integer)
buildSpaceMap (PuzzleState {trees}) = do
  directorySizeCache <- newIORef mempty
  let kvTuples = Map.toList trees
  Map.fromList
    <$> traverse
      (\case (nodeKey, _) -> (nodeKey,) <$> calculateNodeSize directorySizeCache trees nodeKey)
      kvTuples

--  find all of the leaf nodes
--  calculate their sizes
--  then, in order:
--
--  - in ascending order:
--  - calculate sizes for the leaf trees
--  - cache those values somewhere (will I get this for free because Haskell?)

solvePart1 :: Int -> Map Text Int -> Int
solvePart1 limit = Map.foldl' (\acc a -> if a < limit then acc + a else acc) 0

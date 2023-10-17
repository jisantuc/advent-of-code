{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Day7
  ( Directory (..),
    File (..),
    FileTree (..),
    Instruction (..),
    OutputLine (..),
    PuzzleState (..),
    buildState,
    emptyFileTree,
    initialState,
    instructionParser,
    puzzleParser,
    step,
  )
import Parser (parsePuzzle)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Testing (expectParsed)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "day 7" $ do
    describe "parsing" $ do
      it "parses the example puzzle" $
        expectParsed (parsePuzzle puzzleParser examplePuzzle) $ \puzz -> do
          length puzz `shouldBe` 10
      it "parses different cd instructions" $ do
        parsePuzzle instructionParser cdRoot `shouldBe` Right (Cd RootDir)
        parsePuzzle instructionParser cdNamed `shouldBe` Right (Cd $ NamedDir "a")
        parsePuzzle instructionParser cdUp `shouldBe` Right (Cd DotDot)
      it "parses ls output" $
        expectParsed
          (parsePuzzle instructionParser exampleLs)
          (`shouldBe` exampleLsExpectation)
      it "parses consecutive ls outputs" $
        expectParsed
          (parsePuzzle puzzleParser exampleLsThenLs)
          ( `shouldBe`
              [ Ls [DirectoryLine $ NamedDir "a"],
                Ls [DirectoryLine $ NamedDir "a"]
              ]
          )
      it "parses two cds in a row" $
        expectParsed
          (parsePuzzle puzzleParser exampleCdThenCd)
          ( `shouldBe`
              [ Cd $ NamedDir "a",
                Cd $ NamedDir "b"
              ]
          )
      it "parses a cd, then an ls" $
        expectParsed
          (parsePuzzle puzzleParser smallExamplePuzzle)
          ( `shouldBe`
              [ Cd RootDir,
                exampleLsExpectation,
                Cd $ NamedDir "a"
              ]
          )
    describe "solvers" $ do
      describe "assembling contents map" $ do
        it "assembles contents for empty file tree" $ do
          t0 <- initialState
          t1 <- step t0 $ Ls []
          t1
            `shouldBe` ( t0
                           { directoryContents = Map.singleton "RootDir" [],
                             trees = Map.singleton "RootDir" emptyFileTree
                           }
                       )
        it "assembles contents for tree with some content" $
          let outputLine =
                [ FileOutputLine $ File "f1" 12,
                  FileOutputLine $ File "f2" 13,
                  DirectoryLine $ NamedDir "b"
                ]
              instruction = Ls outputLine
           in do
                t0 <- initialState
                t1 <- step t0 instruction
                t1
                  `shouldBe` ( t0
                                 { directoryContents = Map.singleton "RootDir" outputLine,
                                   trees =
                                     Map.singleton "RootDir" $
                                       FileTree
                                         { subTree = Set.singleton "RootDir/b",
                                           files = [File "f1" 12, File "f2" 13],
                                           directory = RootDir
                                         }
                                 }
                             )
      describe "assembling whole puzzle state" $ do
        it "builds state from a simple puzzle" $ do
          instructions <- orFail $ parsePuzzle puzzleParser smallExamplePuzzle
          result <- buildState instructions
          let ( PuzzleState
                  { trees
                  -- workingDirectory,
                  -- directoryContents,
                  -- spaceMap
                  }
                ) = result
          trees
            `shouldBe` Map.singleton
              "RootDir"
              ( FileTree
                  RootDir
                  [File "b.txt" 14848514, File "c.dat" 8504156]
                  (Set.fromList ["RootDir/a", "RootDir/d"])
              )
        it "builds state from the example puzzle" $ do
          -- TODO SOLVE IT HERE WOW SO EXCITING
          -- oh also gotta reimplement fileTreeSize
          -- instructions <- orFail $ parsePuzzle puzzleParser examplePuzzle
          -- result <- buildState instructions
          pass

      describe "instruction handling" $ do
        describe "cd handling" $ do
          it "handles cd .." pass
          it "handles cd /" pass
          it "handles cd a" pass

exampleLsExpectation :: Instruction
exampleLsExpectation =
  Ls
    [ DirectoryLine (NamedDir "a"),
      FileOutputLine (File "b.txt" 14848514),
      FileOutputLine (File "c.dat" 8504156),
      DirectoryLine (NamedDir "d")
    ]

cdRoot :: Text
cdRoot =
  [r|$ cd /
|]

cdNamed :: Text
cdNamed =
  [r|$ cd a
|]

cdUp :: Text
cdUp =
  [r|$ cd ..
|]

exampleLsThenLs :: Text
exampleLsThenLs =
  [r|$ ls
dir a
$ ls
dir a
|]

exampleCdThenCd :: Text
exampleCdThenCd =
  [r|$ cd a
$ cd b
|]

exampleLs :: Text
exampleLs =
  [r|$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
|]

smallExamplePuzzle :: Text
smallExamplePuzzle =
  [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
|]

examplePuzzle :: Text
examplePuzzle =
  [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|]

simpleExampleTree :: FileTree
simpleExampleTree = emptyFileTree {files = [File {fileSize = 10, fileName = "foo"}]}

pass :: Expectation
pass = pure ()

orFail :: (Show e) => Either e a -> IO a
orFail (Right a) = pure a
orFail (Left e) = fail (show e)

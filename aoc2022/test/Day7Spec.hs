{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import Data.Text (Text)
import Day7
  ( Directory (..),
    File (..),
    Instruction (..),
    OutputLine (..),
    instructionParser,
    puzzleParser,
  )
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe, Expectation)
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

pass :: Expectation
pass = True `shouldBe` True

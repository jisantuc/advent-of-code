{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec where

import Data.Either (isRight)
import Data.Text (Text)
import Day7 (Directory (..), File (..), Instruction (..), OutputLine (..), cdParser, lsParser, puzzleParser)
import Parser (parsePuzzle)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "day 7" $
    describe "parsing" $ do
      it "parses the example puzzle" $
        parsePuzzle puzzleParser examplePuzzle `shouldSatisfy` isRight
      it "parses different cd instructions" $ do
        parsePuzzle cdParser cdRoot `shouldBe` Right (Cd RootDir)
        parsePuzzle cdParser cdNamed `shouldBe` Right (Cd $ NamedDir "a")
        parsePuzzle cdParser cdUp `shouldBe` Right (Cd DotDot)
      it "parses ls output" $
        parsePuzzle lsParser exampleLs
          `shouldBe` Right exampleLsExpectation
      it "parses a cd, then an ls" $
        ( \case
            Right result ->
              result
                `shouldBe` [ Cd RootDir,
                             exampleLsExpectation,
                             Cd $ NamedDir "a"
                           ]
            Left e -> fail . errorBundlePretty $ e
        )
          (parsePuzzle puzzleParser smallExamplePuzzle)

exampleLsExpectation :: Instruction
exampleLsExpectation =
  Ls
    [ DirectoryLine (NamedDir "a"),
      FileOutputLine (File "b" 14848514),
      FileOutputLine (File "c" 8504156),
      DirectoryLine (NamedDir "d")
    ]

cdRoot :: Text
cdRoot = "$ cd /"

cdNamed :: Text
cdNamed = "$ cd a"

cdUp :: Text
cdUp = "$ cd .."

exampleLs :: Text
exampleLs =
  [r|$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d|]

smallExamplePuzzle :: Text
smallExamplePuzzle =
  [r|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a|]

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
7214296 k|]

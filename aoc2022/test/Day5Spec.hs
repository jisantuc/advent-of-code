{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Vector (Vector)
import Day5 (Instruction (..), Puzzle (..), fullParser, puzzleParser, solvePart1, solvePart2)
import Lib.Stack (Stack (..), ofAs)
import Parser
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Text.Megaparsec (errorBundlePretty)
import Text.RawString.QQ (r)

spec :: Spec
spec = describe "day 5" $ do
  describe "parsing" $ do
    it "parses the stacks" $
      let parseResult = parsePuzzle puzzleParser exampleStacks
       in ( \case
              Right puzzIO -> assertStacks puzzIO
              Left e -> fail $ errorBundlePretty e
          )
            parseResult
    it
      "parses the example puzzle"
      $ let parseResult = parsePuzzle fullParser exampleFullPuzzle
         in ( \case
                Right (puzzIO, instructions) -> do
                  length instructions `shouldBe` 4
                  do
                    (Puzzle parsedStacks) <- puzzIO
                    (Puzzle testStacks) <- fst examplePuzzleData
                    parsedVecs <- traverse readStackRef parsedStacks
                    testVecs <- traverse readStackRef testStacks
                    parsedVecs `shouldBe` testVecs
                Left e -> fail $ errorBundlePretty e
            )
              parseResult
  describe "solutions" $ do
    it "gets the part 1 example right" $
      let (puzzleIO, instructions) = examplePuzzleData
       in do
            puzzle <- puzzleIO
            solution <- solvePart1 puzzle instructions
            solution `shouldBe` "CMZ"
    it "gets the part 2 example right" $
      let (puzzleIO, instructions) = examplePuzzleData
       in do
            puzzle <- puzzleIO
            solution <- solvePart2 puzzle instructions
            solution `shouldBe` "MCD"

assertStacks :: IO Puzzle -> Expectation
assertStacks puzzIO =
  do
    (Puzzle parsedStacks) <- puzzIO
    (Puzzle testStacks) <- fst examplePuzzleData
    parsedVecs <- traverse readStackRef parsedStacks
    testVecs <- traverse readStackRef testStacks
    parsedVecs `shouldBe` testVecs

readStackRef :: Stack a -> IO (Vector a)
readStackRef (Stack ref) = readIORef ref

examplePuzzleData :: (IO Puzzle, [Instruction])
examplePuzzleData =
  let stack1 = ofAs ['Z', 'N']
      stack2 = ofAs ['M', 'C', 'D']
      stack3 = ofAs ['P']
      stacks = sequence [stack1, stack2, stack3]
      instructions =
        [ Instruction 1 2 1,
          Instruction 3 1 3,
          Instruction 2 2 1,
          Instruction 1 1 2
        ]
   in (Puzzle <$> stacks, instructions)

exampleStacks :: Text
exampleStacks =
  [r|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 |]

exampleFullPuzzle :: Text
exampleFullPuzzle =
  [r|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

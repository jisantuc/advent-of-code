{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10Spec where

import AoC.Data.Grid.Rectangular (RectangularGrid (..))
import AoC.Parser.Testing (expectParsed, expectSuccessfulParse)
import Data.Text (Text)
import Data.Vector ((!))
import Day10 (scorePosition, puzzleParser, solve1, solve2)
import Debug.Trace (traceShow)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Text.RawString.QQ (r)

spec :: Spec
spec =
  describe "Day10Spec" $
    let parsedSimplePuzzle = parse puzzleParser "" simpleTopo
        parsedMediumPuzzle = parse puzzleParser "" mediumTopo
        parsedBigPuzzle = parse puzzleParser "" bigTopo
        parsedSimpleForkingPuzzle = parse puzzleParser "" simpleForkingTopo
        parsedMediumForkingPuzzle = parse puzzleParser "" mediumForkingTopo
        parsedSuperForkingPuzzle = parse puzzleParser "" superForkingTopo
     in do
          describe "parsing" $ do
            it "parses the simple puzzle" $ do
              expectSuccessfulParse parsedSimplePuzzle False
              expectParsed parsedSimplePuzzle $ \(RectangularGrid puzz) -> do
                puzz ! 0 ! 3 `shouldBe` 0
                puzz ! 1 ! 3 `shouldBe` 1
                puzz ! 3 ! 4 `shouldBe` 4
                puzz ! 6 ! 6 `shouldBe` 9
                puzz ! 6 ! 0 `shouldBe` 9
          describe "part 1" $ do
            describe "trailhead scoring" $ do
              it "scores a summit as 1" $
                expectParsed (parse puzzleParser "" "9") $ \puzz ->
                  scorePosition 0 0 puzz `shouldBe` 1
              it "scores a location next to a summit as 1" $ do
                expectParsed (parse puzzleParser "" "98") $ \puzz ->
                  traceShow puzz $ scorePosition 0 0 puzz `shouldBe` 1
                expectParsed (parse puzzleParser "" "89") $ \puzz ->
                  traceShow puzz $ scorePosition 0 0 puzz `shouldBe` 1
              it "scores a location between two summits as 2" $
                expectParsed (parse puzzleParser "" "989") $ \puzz ->
                  scorePosition 0 1 puzz `shouldBe` 2
              it "scores a location between four summits as 4" $
                expectParsed (parse puzzleParser "" "090\n989\n090") $ \puzz ->
                  scorePosition 1 1 puzz `shouldBe` 4
              it "scores the simple trailhead correctly" $
                expectParsed parsedSimplePuzzle $
                  (`shouldBe` 2) . scorePosition 0 3
              it "counts reachable summits in the medium topo" $
                expectParsed parsedMediumPuzzle $ \puzz -> do
                  scorePosition 0 1 puzz `shouldBe` 1
                  scorePosition 6 5 puzz `shouldBe` 2
              it "counts reachable summits in the big topo" $
                expectParsed parsedBigPuzzle $ \puzz -> do
                  scorePosition 0 2 puzz `shouldBe` 5
                  scorePosition 0 4 puzz `shouldBe` 6
            it "solves the examples" $ do
              expectParsed parsedBigPuzzle $ \puzz -> solve1 puzz `shouldBe` 36
          describe "part 2" $ 
            it "rates trailheads correctly" $ do
              expectParsed parsedSimpleForkingPuzzle $ \puzz ->
                solve2 puzz `shouldBe` 3
              expectParsed parsedMediumForkingPuzzle $ \puzz ->
                solve2 puzz `shouldBe` 13
              expectParsed parsedSuperForkingPuzzle $ \puzz ->
                solve2 puzz `shouldBe` 227
              expectParsed parsedBigPuzzle $ \puzz ->
                solve2 puzz `shouldBe` 81

simpleTopo :: Text
simpleTopo =
  [r|...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
|]

mediumTopo :: Text
mediumTopo =
  [r|10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01|]

bigTopo :: Text
bigTopo =
  [r|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
|]

simpleForkingTopo :: Text
simpleForkingTopo =
  [r|.....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....
|]

mediumForkingTopo :: Text
mediumForkingTopo = [r|..90..9
...1.98
...2..7
6543456
765.987
876....
987....
|]

superForkingTopo :: Text
superForkingTopo = [r|012345
123456
234567
345678
4.6789
56789.
|]

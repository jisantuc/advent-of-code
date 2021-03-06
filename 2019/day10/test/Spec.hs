import qualified Data.ByteString as BS

main :: IO ()
main = putStrLn "Test suite not yet implemented"

testPuzzleSmall :: BS.ByteString
testPuzzleSmall =
  [q|.#..#
.....
#####
....#
...##|]

testPuzzleMedium1 :: BS.ByteString
testPuzzleMedium1 =
  [q|......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####|]

testPuzzleMedium2 :: BS.ByteString
testPuzzleMedium2 =
  [q|#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.|]

testPuzzleMedium3 :: BS.ByteString
testPuzzleMedium3 =
  [q|.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..|]

testPuzzleLarge :: BS.ByteString
testPuzzleLarge =
  [q|.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|]

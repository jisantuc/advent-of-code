module Day3 where

import Data.Bits (shiftR, Bits (bit), (.&.))
import Parser (Parser)
import Text.Megaparsec.Char.Lexer (binary)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol)
import Data.Foldable (foldl')

type Puzzle = [Int]

puzzleParser :: Parser Puzzle
puzzleParser = binary `sepEndBy` eol

-- procedure:
-- for each Int (int representation of the binary parsed value),
-- bit shift it such that we lose all values to the right of it,
-- then MOD 2 
findGammaBit :: Puzzle -> Int -> Bool
findGammaBit puzz shift =
    let
        numberOfExamples = length puzz
        sumOfBits = foldl' (\acc n -> (shiftR n shift .&. 1 + acc)) 0 puzz
    in
        sumOfBits > numberOfExamples `div` 2

invertBit :: Bool -> Bool
invertBit = not

bitSum :: [Bool] -> Int
bitSum [] = 0
bitSum bools = 
    let indexed = zipWithIndex bools
    in foldr (\(idx, b) acc ->
            if b then acc + bit idx else acc
        ) 0 indexed

zipWithIndexSizedArr :: Int -> [a] -> [(Int, a)]
zipWithIndexSizedArr _ [] = []
zipWithIndexSizedArr n (x:xs) = (n - 1,x) : zipWithIndexSizedArr (n - 1) xs

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex [] = []
zipWithIndex ns =
    zipWithIndexSizedArr (length ns) ns

solvePart1 :: Puzzle -> Int
solvePart1 [] = 1
solvePart1 ns =
    let
        gammaBits = findGammaBit ns <$> reverse [0 .. 4]
        epsilonBits = invertBit <$> gammaBits
        gammaSum = bitSum gammaBits
        epsilonSum = bitSum epsilonBits
    in
        gammaSum * epsilonSum

solve :: Puzzle -> Int
solve = solvePart1
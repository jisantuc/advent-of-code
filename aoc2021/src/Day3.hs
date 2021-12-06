module Day3 where

import Data.Bits (shiftR, Bits (bit), (.&.))
import Parser (Parser)
import Text.Megaparsec.Char.Lexer (binary)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (eol)
import Data.Foldable (foldl')
import Data.Maybe (listToMaybe, fromMaybe)

type Puzzle = [Int]

-- Sad because all the coercions were getting messy :(
sadExp :: Int -> Int -> Int
sadExp _ 0 = 1
sadExp b pow = b * sadExp b (pow - 1)

intLog :: Int -> Int
intLog x
  | x <= 1 = 0
  | otherwise = go 1 x
    where go n v = if sadExp 2 n > v then n - 1 else go (n + 1) x

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
        2 * sumOfBits >= numberOfExamples

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
        -- need a value with the most significant bit set
        -- we don't know in advance what that bit is likely to be, but any value
        -- with the most significant bit set is going to be greater than values
        -- without that bit set
        maxValue = maximum ns
        gammaBits = findGammaBit ns <$> reverse [0 .. intLog maxValue]
        epsilonBits = invertBit <$> gammaBits
        gammaSum = bitSum gammaBits
        epsilonSum = bitSum epsilonBits
    in
        gammaSum * epsilonSum

solvePart2 :: Puzzle -> Maybe Int 
solvePart2 [] = Nothing
solvePart2 ns =
    let
        maxValue = maximum ns
        bitsToCheck = reverse [ 0 .. intLog maxValue ]
        matchesBit x b gamma = (shiftR x b .&. 1 == 1 && gamma) || (shiftR x b .&. 1 == 0 && not gamma)
        compareBits boolF f = foldl' (\acc b ->
            case acc of
                [x] -> [x]
                arr ->
                    let gammaBit = boolF $ findGammaBit arr b
                    in filter (\x -> f x b gammaBit) arr ) 
        oxygenRating = compareBits id matchesBit ns bitsToCheck
        scrubberRating = compareBits invertBit matchesBit ns bitsToCheck
    in
        (*) <$> listToMaybe scrubberRating <*> listToMaybe oxygenRating

solve :: Puzzle -> Int
solve = fromMaybe 1 . solvePart2
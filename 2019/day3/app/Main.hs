module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.Foldable
import           Data.Sequence
import qualified Data.Set                         as Set
import           Lib

toSet :: Ord a => Seq a -> Set.Set a
toSet Empty      = Set.empty
toSet (x :<| xs) = Set.singleton x `Set.union` toSet xs

main :: IO ()
main = do
  inText <- BS.readFile "puzzle.txt"
  let parsed = parseOnly (many' puzzleLineParser) inText
  case parsed of
    Left err -> error err
    Right wires ->
      let histories = interpretInstructions Empty <$> wires
          pointSets = toSet <$> histories
          sharedPoints = foldl' Set.intersection (head pointSets) pointSets
       in print . show $
          Set.foldl'
            (\x y ->
               if (manhattanDistance x < manhattanDistance y && x /= (0, 0))
                 then x
                 else y)
            (0, 0)
            sharedPoints

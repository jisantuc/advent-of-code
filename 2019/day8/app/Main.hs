module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.Sort                        (sortBy)

import           Lib

main :: IO ()
main = do
  bs <- BS.readFile "puzzle.txt"
  let layers = parseOnly rastersParser bs
  case layers of
    Right rasts ->
      let stats = getRasterStats <$> rasts
      in
        print $ "Most zeros layer: " ++ show (head . sortBy (\x y -> compare (fst x) (fst y)) $ stats)
    Left e ->
      print $ "Failed to parse: " ++ e

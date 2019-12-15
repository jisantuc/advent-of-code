module Main where

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS
import           Data.List                        (foldl')
import           Data.Sort                        (sortBy)

import           Lib

main :: IO ()
main = do
  bs <- BS.readFile "puzzle.txt"
  let layers = parseOnly rastersParser bs
  case layers of
    Right rasts ->
      let
        stats = getRasterStats <$> rasts
        sorted = sortBy (\x y -> compare (fst x) (fst y)) $ stats
        sortedMsg = show . head $ sorted
        (InterleavedRaster combined) =
          case rasts of
            (r:rs) ->
              foldl' combineRasters r rs
            _ ->
              error "what"
      in
        do
          print $ ("Most zeros layer: " ++ sortedMsg)
          print $ "Final message: " ++ show (resolveRow <$> combined)
    Left e ->
      print $ "Failed to parse: " ++ e

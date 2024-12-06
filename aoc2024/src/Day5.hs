{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import AoC.Parser (Parser)
import Data.List (unsnoc)
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Text.Megaparsec (sepBy, sepEndBy, skipSome)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (decimal)

newtype PrecedenceRules = PrecedenceRules {unPrecedenceRules :: Map.Map Int (Set.Set Int)} deriving (Eq, Show)

data Puzzle = Puzzle {rules :: PrecedenceRules, updates :: [[Int]]} deriving (Eq, Show)

precedenceParser :: Parser PrecedenceRules
precedenceParser =
  PrecedenceRules . Map.unionsWith mappend
    <$> (Map.singleton <$> (decimal <* "|") <*> (Set.singleton <$> decimal)) `sepEndBy` eol

updatesParser :: Parser [[Int]]
updatesParser = (decimal `sepBy` ",") `sepBy` eol

puzzleParser :: Parser Puzzle
puzzleParser = do
  rules <- precedenceParser
  skipSome eol
  updates <- updatesParser
  pure $ Puzzle {rules, updates}

midpoint :: [a] -> Maybe a
midpoint [] = Nothing
midpoint [x] = Just x
midpoint [x, _] = Just x
midpoint xs = Just $ xs !! (length xs `div` 2)

updateIsValid :: PrecedenceRules -> [Int] -> Bool
updateIsValid (PrecedenceRules rules) update =
  foldr
    ( \(idx, page) result ->
        let rule = Map.lookup page rules
         in case rule of
              Just followers ->
                let earlierPages = Set.fromList (take idx update)
                 in result && Set.null (Set.intersection earlierPages followers)
              Nothing -> result
    )
    True
    (zip [0 ..] update)

fixInvalidLine :: PrecedenceRules -> [Int] -> [Int]
fixInvalidLine r@(PrecedenceRules rules) = go []
  where
    go :: [Int] -> [Int] -> [Int]
    go acc remaining =
      case unsnoc remaining of
        Nothing -> if updateIsValid r acc then acc else go [] acc
        Just (h, page) ->
          let rule = Map.lookup page rules
           in case rule of
                Nothing -> go (page : acc) h
                Just followers ->
                  if Set.null (Set.intersection (Set.fromList h) followers)
                    then
                      go (page : acc) h
                    else
                      let violation = head $ filter (`elem` followers) h
                          hWithoutLatestViolation = filter (/= violation) h
                       in go (violation : acc) (hWithoutLatestViolation <> [page])

solve1 :: Puzzle -> Int
solve1 (Puzzle {rules, updates}) =
  let validUpdates = filter (updateIsValid rules) updates
   in getSum . mconcat $ (maybe mempty Sum . midpoint <$> validUpdates)

solve2 :: Puzzle -> Int
solve2 (Puzzle {rules, updates}) =
  let invalidUpdates = filter (not . updateIsValid rules) updates
   in getSum . mconcat $
        (maybe mempty Sum . midpoint . fixInvalidLine rules <$> invalidUpdates)

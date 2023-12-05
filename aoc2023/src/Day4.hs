{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day4 where

import AoC.Parser (Parser)
import Control.Monad (void)
import Data.Foldable (foldMap', foldl')
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import Text.Megaparsec (MonadParsec (eof), between, many, sepBy, sepEndBy, skipMany, (<|>))
import Text.Megaparsec.Char (eol, hspace)
import Text.Megaparsec.Char.Lexer (decimal)

data Card = Card
  { winningNumbers :: Set.Set Int,
    myNumbers :: Set.Set Int,
    cardId :: Int
  }
  deriving (Eq, Show)

newtype Puzzle = Puzzle [Card] deriving (Eq, Show)

cardParser :: Parser Card
cardParser = do
  "Card " *> skipMany " "
  cardNum <- decimal
  void ":"
  winningNums <- Set.fromList <$> between hspace "|" (decimal `sepEndBy` hspace)
  myNums <- Set.fromList <$> between hspace (void eol <|> eof) (decimal `sepBy` hspace)
  pure $ Card winningNums myNums cardNum

parser :: Parser Puzzle
parser = Puzzle <$> many cardParser

pointsForCard :: Card -> Sum Int
pointsForCard (Card {winningNumbers, myNumbers}) =
  let myWinningNumbers = Set.toList $ Set.intersection winningNumbers myNumbers
      powersOfTwo = (2 ^) <$> ([0, 1 ..] :: [Int])
      pointsForEachMatch = zipWith (\_ x -> x) myWinningNumbers powersOfTwo
   in if null pointsForEachMatch then mempty else Sum . maximum $ pointsForEachMatch

solver1 :: Puzzle -> Int
solver1 (Puzzle cards) = getSum $ foldMap' pointsForCard cards

solver2 :: Puzzle -> Int
solver2 (Puzzle cards) =
  let cardCountsMap = foldMap' (`Map.singleton` 1) $ cardId <$> cards
   in sum . Map.elems $
        foldl'
          ( \acc (Card {cardId, winningNumbers, myNumbers}) ->
              let Just count = Map.lookup cardId acc
                  myWinningNumbers = Set.intersection winningNumbers myNumbers
                  howManyFutureCards = length myWinningNumbers
                  increment = Map.fromList $ (,count) <$> [cardId + 1 .. cardId + howManyFutureCards]
               in Map.unionWith (+) acc increment
          )
          cardCountsMap
          cards

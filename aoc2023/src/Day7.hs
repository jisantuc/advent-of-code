{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day7 where

import AoC.Parser (Parser)
import qualified AoC.Parser as Parser
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldMap'))
import Data.Functor ((<&>))
import Data.List (sort)
import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import Text.Megaparsec (many, sepEndBy, (<|>))
import Text.Megaparsec.Char (digitChar, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data HandType
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Show, Ord)

data Hand = Hand
  { cards :: [Integer],
    bid :: Integer
  }
  deriving (Eq, Show)

newtype JokerHand = JokerHand {getHand :: Hand} deriving (Eq, Show)

instance Ord JokerHand where
  compare (JokerHand h1@(Hand {cards = c1})) (JokerHand h2@(Hand {cards = c2})) =
    let replaceJokers n = case n of
          11 -> 0
          other -> other
     in case classifyHandWithJoker h1 `compare` classifyHandWithJoker h2 of
          EQ ->
            head $ dropWhile (== EQ) $ zipWith compare (c1 <&> replaceJokers) (c2 <&> replaceJokers)
          ord -> ord

instance Ord Hand where
  compare h1@(Hand {cards = c1}) h2@(Hand {cards = c2}) =
    case classifyHand h1 `compare` classifyHand h2 of
      EQ ->
        head $ dropWhile (== EQ) $ zipWith compare c1 c2
      ord -> ord

newtype Puzzle = Puzzle [Hand] deriving (Eq, Show)

classifyHand :: Hand -> HandType
classifyHand (Hand {cards}) =
  let cardCounts = Map.fromListWith (+) $ (,1 :: Integer) <$> cards
   in case sort (Map.elems cardCounts) of
        [1, 1, 1, 1, 1] -> HighCard
        [1, 1, 1, 2] -> OnePair
        [1, 2, 2] -> TwoPairs
        [1, 1, 3] -> ThreeOfAKind
        [2, 3] -> FullHouse
        [1, 4] -> FourOfAKind
        [5] -> FiveOfAKind
        _ -> HighCard

classifyHandWithJoker :: Hand -> HandType
classifyHandWithJoker (Hand {cards}) =
  let cardCounts = Map.fromListWith (+) $ (,1 :: Integer) <$> cards
      numJokers = length (filter (== 11) cards)
      hasJoker = numJokers > 0
   in case sort (Map.elems cardCounts) of
        -- it doesn't matter whether there are jokers or not if you have
        -- five of a kind
        [5] -> FiveOfAKind
        -- these hands only have one upgrade path if a joker is present
        [1, 1, 1, 1, 1] -> if hasJoker then OnePair else HighCard
        [1, 1, 1, 2] -> if hasJoker then ThreeOfAKind else OnePair
        [1, 1, 3] -> if hasJoker then FourOfAKind else ThreeOfAKind
        [1, 4] -> if hasJoker then FiveOfAKind else FourOfAKind
        [2, 3] -> if hasJoker then FiveOfAKind else FullHouse
        -- for this hand it matters whether the number of jokers is 1 or 2
        -- if it's one, then it's a full house (upgrade one of the pairs)
        -- if it's two, then it's four of a kind (match the other pair)
        [1, 2, 2] ->
          if numJokers == 1
            then FullHouse
            else if numJokers == 2 then FourOfAKind else TwoPairs
        _ -> HighCard

parser :: Parser Puzzle
parser =
  let highCardParser =
        Parser.fromMap $
          Map.fromList
            [ ("A", 14),
              ("K", 13),
              ("Q", 12),
              ("J", 11),
              ("T", 10)
            ]
      cardsParser = many ((digitChar <&> fromIntegral . digitToInt) <|> highCardParser)
      handParser = Hand <$> (cardsParser <* " ") <*> decimal
   in Puzzle <$> handParser `sepEndBy` eol

solver1 :: Puzzle -> Integer
solver1 (Puzzle hands) =
  getSum $
    foldMap'
      (\(Hand {bid}, rank) -> Sum $ rank * bid)
      (zip (sort hands) [1 ..])

solver2 :: Puzzle -> Integer
solver2 (Puzzle hands) =
  let sortedHands = (sort $ JokerHand <$> hands)
   in getSum $
        foldMap'
          ( \(JokerHand (Hand {bid}), rank) ->
              Sum $ rank * bid
          )
          (zip sortedHands [1 ..])

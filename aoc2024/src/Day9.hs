{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day9 where

import AoC.Parser (Parser)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Text.Megaparsec (eof, (<|>))
import Text.Megaparsec.Char (digitChar, eol)

data Disk
  = File
      { fileId :: Int,
        fileSize :: Int
      }
  | EmptySpace {emptyLength :: Int}
  deriving (Eq, Show)

isEmptySpace :: Disk -> Bool
isEmptySpace (EmptySpace _) = True
isEmptySpace _ = False

size :: Disk -> Int
size (File {fileSize}) = fileSize
size (EmptySpace {emptyLength}) = emptyLength

debugShow :: [Disk] -> String
debugShow disk =
  go "" (uncompress disk)
  where
    go acc [] = acc
    go acc (File {fileId} : t) = go (acc <> show fileId) t
    go acc (EmptySpace _ : t) = go (acc <> ".") t

puzzleParser :: Parser [Disk]
puzzleParser =
  go 0 (File 0) []
  where
    go nextId f acc =
      (eol $> acc)
        <|> (eof $> acc)
        <|> ( digitChar
                >>= ( \case
                        disk@(File {fileId}) -> go (fileId + 1) EmptySpace (acc <> [disk])
                        disk@(EmptySpace _) -> go nextId (File nextId) (acc <> [disk])
                    )
                  . f
                  . digitToInt
            )

uncompress :: [Disk] -> [Disk]
uncompress [] = []
uncompress (f@(File {fileSize}) : disk) = replicate fileSize f <> uncompress disk
uncompress (f@(EmptySpace {emptyLength}) : disk) = replicate emptyLength f <> uncompress disk

checkSum :: [Disk] -> Int
checkSum =
  foldl'
    ( \acc (idx, diskSpace) ->
        case diskSpace of
          File {fileId} -> acc + fileId * idx
          _ -> acc
    )
    0
    . zip [0 ..]

solve2 :: [Disk] -> Int
solve2 diskSpace =
  checkSum $
    uncompress (go [] diskSpace)
  where
    go acc [] = acc
    go acc (h@(File _ _) : t) = go (acc <> [h]) t
    go acc ((EmptySpace emptyLength) : t) =
      let (toAcc, toMove, emptySpace) =
            foldr
              ( \disk (newFiles, stillToCheck, remainingSize) ->
                  case disk of
                    es@(EmptySpace _) -> (newFiles, es : stillToCheck, remainingSize)
                    f@(File {fileSize}) ->
                      if fileSize <= remainingSize
                        then (newFiles <> [f], EmptySpace fileSize : stillToCheck, remainingSize - fileSize)
                        else (newFiles, f : stillToCheck, remainingSize)
              )
              ([], [], emptyLength)
              t
       in go (acc <> toAcc <> [EmptySpace emptySpace]) toMove

solve1 :: [Disk] -> Int
solve1 diskSpace =
  checkSum $ go False [] uncompressed reversedUncompressed
  where
    uncompressed = uncompress diskSpace
    reversedUncompressed =
      reverse
        (filter (not . isEmptySpace) uncompressed)
    filledBlocks = length reversedUncompressed
    go True acc _ _ = acc
    go _ acc [] _ = acc
    go False acc (EmptySpace _ : _) [] = acc
    go False acc (File _ _ : _) [] = acc
    go False acc (h@(File _ _) : t) un =
      let newAcc = acc <> [h]
       in go (length newAcc == filledBlocks) newAcc t un
    go False acc _ (EmptySpace _ : _) = acc
    go False acc (EmptySpace _ : t) (unH : unT) = go False (acc <> [unH]) t unT

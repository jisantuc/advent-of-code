{-# LANGUAGE OverloadedStrings #-}

module Main where

import AoC.Net.Puzzle (Config (..), fetch)
import qualified AoC.Parser.CLI as CLI
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.ByteString.Char8 as BS
import Day10 (puzzleParser, solve1)
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    (<**>),
  )
import System.Environment (lookupEnv)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  Just sessionToken <- lookupEnv "AOC_SESSION_COOKIE"
  runnerConf <- execParser opts
  let conf =
        Config
          { year = CLI.year runnerConf,
            day = CLI.day runnerConf,
            token = BS.pack sessionToken,
            cacheFile = "puzzles/puzzle" <> show (CLI.day runnerConf) <> ".txt"
          }
  puzzleText <- runReaderT fetch conf
  case parse puzzleParser "" puzzleText of
    Right puzz -> do
      print ("Answer: " <> show (solve1 puzz))
    Left e -> print $ errorBundlePretty e
  where
    opts =
      info
        (CLI.runnerOptionsParser <**> helper)
        (fullDesc <> progDesc "Solve a puzzle and print the output for some year and day")

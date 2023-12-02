{-# LANGUAGE OverloadedStrings #-}

module Main where

import AoC.Net.Puzzle (Config (..), fetch)
import qualified AoC.Parser.CLI as CLI
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString.Char8 (pack, toStrict)
import Options.Applicative
  ( execParser,
    fullDesc,
    helper,
    info,
    progDesc,
    (<**>),
  )
import System.Environment (lookupEnv)
import Text.Megaparsec (errorBundlePretty, runParser)
import Day2 ( day2Parser, solve2 )

main :: IO ()
main = do
  Just sessionToken <- lookupEnv "AOC_SESSION_COOKIE"
  runnerConf <- execParser opts
  let conf =
        Config
          { year = CLI.year runnerConf,
            day = CLI.day runnerConf,
            token = pack sessionToken,
            cacheFile = "puzzle.txt"
          }
  puzzleBytestring <- toStrict <$> runReaderT fetch conf
  case runParser day2Parser "" puzzleBytestring of
    Right puzz -> print $ solve2 puzz
    Left e -> print $ errorBundlePretty e
  where
    opts = info (CLI.runnerOptionsParser <**> helper) (fullDesc <> progDesc "Solve a puzzle and print the output for some year and day")

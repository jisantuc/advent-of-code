module AoC.Parser.Testing
  ( expectSuccessfulParse,
    expectParsed,
    expectParsedIO,
  )
where

import Control.Monad (when)
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

expectSuccessfulParse :: (Show a) => Either (ParseErrorBundle Text Void) a -> Bool -> Expectation
expectSuccessfulParse (Left err) _ = fail . errorBundlePretty $ err
expectSuccessfulParse (Right parsed) debug = when debug $ print $ show parsed

expectParsed :: Either (ParseErrorBundle Text Void) t -> (t -> Expectation) -> Expectation
expectParsed parseResult f = case parseResult of
  Right puzz -> f puzz
  Left e -> fail . errorBundlePretty $ e

expectParsedIO :: IO (Either (ParseErrorBundle Text Void) t) -> (t -> Expectation) -> Expectation
expectParsedIO mParseResult f =
  mParseResult >>= (`expectParsed` f)

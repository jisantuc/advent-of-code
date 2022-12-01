module Testing where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

expectSuccessfulParse :: Show a => Either (ParseErrorBundle Text Void) a -> Bool -> Expectation
expectSuccessfulParse (Left err) _ = fail $ errorBundlePretty err
expectSuccessfulParse (Right parsed) debug = if debug then (print $ show parsed) else pure ()

expectParsed :: Either (ParseErrorBundle Text Void) t -> (t -> Expectation) -> Expectation
expectParsed parseResult f = case parseResult of
  Right puzz -> f puzz
  Left _ -> fail "Parsing failed, see the parsing test for more details"

expectParsedIO :: IO (Either (ParseErrorBundle Text Void) t) -> (t -> Expectation) -> Expectation
expectParsedIO mParseResult f =
  mParseResult >>= (\result -> expectParsed result f)

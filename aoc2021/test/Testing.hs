module Testing where

import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (Expectation)
import Text.Megaparsec (ParseErrorBundle)

expectParsed :: Either (ParseErrorBundle Text Void) t -> (t -> Expectation) -> Expectation
expectParsed parseResult f = case parseResult of
  Right puzz -> f puzz
  Left _ -> fail "Parsing failed, see the parsing test for more details"

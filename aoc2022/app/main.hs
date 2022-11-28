module Main (main) where
import Parser (foo)

main :: IO ()
main = print $ foo 3

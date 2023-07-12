module Main (main) where

import Json (json, runParser, ParseError (..))
import System.Exit (exitFailure)

main :: IO ()
main = do
  s <- getContents
  case runParser json s of
    Left e -> putStrLn (toMessage e) >> exitFailure
    Right v -> putStrLn $ show v

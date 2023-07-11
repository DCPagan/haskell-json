module Main (main) where

import Json (json, runParser)

main :: IO ()
main = do
  s <- getContents
  case runParser json s of
    Left e -> ioError e
    Right v -> putStrLn $ show v

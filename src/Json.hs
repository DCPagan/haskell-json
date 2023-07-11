module Json
  ( JsonValue,
    Parser,
    parse,
    runParser,
    item,
    sat,
    char,
    string,
    escape,
    space,
    digit,
    natural,
    integer,
    float,
    token,
    manyTokens,
    json,
    jsonNull,
    jsonBool,
    jsonInteger,
    jsonFloat,
    jsonString,
    jsonArray,
    jsonKeyValuePair,
    jsonObject,
  )
where

import Control.Applicative (Alternative, empty, liftA2, many, some, (<|>))
import Control.Monad (void)
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.List (intercalate)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonFloat Double
  | JsonInteger Integer
  | JsonString { toString :: String }
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq)

newtype Parser a = P {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f p = P (\s -> parse p s >>= \(v, s') -> [(f v, s')])

instance Applicative Parser where
  pure v = P (\s -> [(v, s)])
  pf <*> px = P (\s -> parse pf s >>= \(f, s') -> parse (f <$> px) s')

instance Monad Parser where
  p >>= f = P (\s -> parse p s >>= \(v, s') -> parse (f v) s')

instance Alternative Parser where
  empty = P (const [])
  p <|> q =
    P
      ( \s -> case parse p s of
          [] -> parse q s
          ss -> ss
      )

instance Show JsonValue where
  show value = case value of
    JsonNull -> "null"
    JsonBool True -> "true"
    JsonBool False -> "false"
    JsonFloat x -> show x
    JsonInteger x -> show x
    JsonString x -> x
    JsonArray xs -> '[' : foldr ((++) . show) "]" xs
    JsonObject xs ->
      '{'
        : intercalate "," (map (\(k, v) -> k ++ ':' : show v) xs)
        ++ "}"

runParser :: Parser a -> String -> Either IOError a
runParser p s = case parse p s of
  [] -> Left $ userError "Syntax error"
  [(v, [])] -> Right v
  [(_, t)] -> Left $ userError $ "Trailing characters: " ++ t
  _ -> Left $ userError "Ambiguous parse"

item :: Parser Char
item =
  P
    ( \s -> case s of
        [] -> []
        x : xs -> [(x, xs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

char :: Char -> Parser Char
char = sat . (==)

string :: String -> Parser String
string = foldr (liftA2 (:) . sat . (==)) (pure [])

times :: Int -> Parser a -> Parser [a]
times 0 _ = pure []
times c p = (:) <$> p <*> times (pred c) p

space :: Parser ()
space = void $ many $ sat isSpace

digit :: Parser Char
digit = sat isDigit

escape :: Parser String
escape =
  (:)
    <$> char '\\'
    <*> ( (:) <$> sat (flip elem "\"\\/bfnrt") <*> pure []
            <|> (:) <$> (sat ('u' ==)) <*> (times 4 (sat isHexDigit))
        )

natural :: Parser Integer
natural = some digit >>= return . read

integer :: Parser Integer
integer = (char '-' >> negate <$> natural) <|> natural

float :: Parser Double
float = do
  s <- (char '-' >> return ((-) . negate)) <|> pure (+)
  x <- natural >>= return . fromIntegral
  char '.'
  y <- some digit
  let m = fromIntegral $ read y :: Double
  let e = 10 ^ length y :: Double
  return $ s x $ m / e

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

manyTokens :: Parser a -> Parser [a]
manyTokens p = (:) <$> p <*> (many $ token (char ',') >> p) <|> pure []

json :: Parser JsonValue
json =
  jsonNull
    <|> jsonBool
    <|> jsonFloat
    <|> jsonInteger
    <|> jsonString
    <|> jsonArray
    <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = token (string "null") >> return JsonNull

jsonBool :: Parser JsonValue
jsonBool =
  (token (string "true") >> return (JsonBool True))
    <|> (token (string "false") >> return (JsonBool False))

jsonInteger :: Parser JsonValue
jsonInteger = token integer >>= return . JsonInteger

jsonFloat :: Parser JsonValue
jsonFloat = token float >>= return . JsonFloat

jsonString :: Parser JsonValue
jsonString = do
  token $ char '"'
  s <- concat <$> (many $ escape <|> (: []) <$> sat ('"' /=))
  token $ char '"'
  return $ JsonString $ '"' : s ++ '"' : []

jsonArray :: Parser JsonValue
jsonArray = do
  token $ char '['
  v <- manyTokens json
  token $ char ']'
  return $ JsonArray v

jsonKeyValuePair :: Parser (String, JsonValue)
jsonKeyValuePair = do
  k <- jsonString
  token $ char ':'
  v <- json
  return (toString k, v)

jsonObject :: Parser JsonValue
jsonObject = do
  token $ char '{'
  pairs <- manyTokens jsonKeyValuePair
  token $ char '}'
  return $ JsonObject pairs

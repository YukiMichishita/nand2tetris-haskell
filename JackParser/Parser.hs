module Parser (Parser, parse, item, string, symbol, oneOf, mapping, natural, satToken, ident, comment, token, sat) where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(v, out)] -> parse (fmap v px) out
      )

instance Monad Parser where
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> []
        (x : xs) -> [(x, xs)]
    )

instance Alternative Parser where
  empty = P (\inp -> [])
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

sat :: (Char -> Bool) -> Parser Char
sat p =
  do
    x <- item
    if p x then return x else empty

satToken :: (Char -> Bool) -> Parser Char
satToken = token . sat

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

strings :: (Foldable t, Alternative f, Monad f, Functor t) => t [a] -> f [a]
strings s = foldl (<|>) (return []) (fmap return s)

mapping :: [(String, a)] -> Parser a
mapping kv =
  let get (k, v) = do
        string k
        return v
   in foldl
        (<|>)
        (empty)
        (fmap get kv)

until :: String -> Parser String
until [] = P (\inp -> [(inp, "")])
until (x : xs) =
  do
    do
      string (x : xs)
      return []
    <|> do
      i <- item
      left <- Parser.until (x : xs)
      return (i : left)

ident :: Parser String
ident = token $ do
  x <- letter
  xs <- many (alphanum <|> char '_')
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  sat isSpace
  return ()

newLine :: Parser ()
newLine = do
  string "\n"
  return ()

comment :: Parser ()
comment =
  do
    do
      string "//"
      many (sat (/= '\n'))
      return ()
    <|> do
      string "/**"
      Parser.until "*/"
      return ()

ignore :: Parser ()
ignore = do
  many (space <|> newLine <|> comment)
  return ()

token :: Parser b -> Parser b
token p = do
  ignore
  v <- p
  ignore
  return v

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

oneOf :: (Foldable t, Functor t) => t [a] -> Parser [a]
oneOf s = token (strings s)

nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

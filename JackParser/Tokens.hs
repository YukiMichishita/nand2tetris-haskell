module Tokens (Keyword, keyword, Symbol, Tokens.symbol, IntegerConstant, integerConstant, StringConstant, stringConstant, Identifier, identifier, ClassName, VarName, SubroutineName) where

import Control.Applicative
import Parser

data Keyword = Kw String deriving (Show)

keyword :: Parser Keyword
keyword = do
  k <-
    oneOf
      [ "class",
        "constructor",
        "function",
        "method",
        "field",
        "static",
        "var",
        "int",
        "char",
        "boolean",
        "void",
        "true",
        "false",
        "null",
        "this",
        "let",
        "do",
        "if",
        "else",
        "while",
        "return"
      ]
  return $ Kw k

data Symbol = Sym String deriving (Show)

symbol :: Parser Symbol
symbol = do
  p <- oneOf ["{", "}", "(", ")", "[", "]", ".", ",", ";", "+", "-", "*", "/", "&", "|", "<", ">", "=", "~"]
  return $ Sym p

data IntegerConstant = IntegerConstant Int deriving (Show)

integerConstant :: Parser IntegerConstant
integerConstant = fmap IntegerConstant natural

data StringConstant = StringConstant String deriving (Show)

stringConstant :: Parser StringConstant
stringConstant = token $ do
  string ['"']
  c <- many (sat (/= '"'))
  string ['"']
  return $ StringConstant c

data Identifier = Id String deriving (Show)

identifier :: Parser Identifier
identifier = fmap Id $ ident

type ClassName = Identifier

type SubroutineName = Identifier

type VarName = Identifier

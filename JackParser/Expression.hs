module Expression where

import Control.Applicative
import Parser
import Tokens

data OpAndTerm = OpAndTerm Op Term deriving (Show)

opAndTerm :: Parser OpAndTerm
opAndTerm = do
  o <- op
  t <- term
  return $ OpAndTerm o t

data Expression
  = Expression
  { term' :: Term,
    opAndTerms :: [OpAndTerm]
  }
  deriving (Show)

expression :: Parser Expression
expression = do
  tm <- term
  ots <- many $ opAndTerm
  return Expression {term' = tm, opAndTerms = ots}

data Term
  = IntegerConstant IntegerConstant
  | StringConstant StringConstant
  | KeywordConstant KeywordConstant
  | VarName' VarName
  | VarNameExpression VarName Expression
  | SubroutineCall' SubroutineCall
  | Expression' Expression
  | UnaryOp UnaryOp Term
  deriving (Show)

term :: Parser Term
term =
  do
    fmap IntegerConstant integerConstant
    <|> fmap StringConstant stringConstant
    <|> fmap KeywordConstant keywordConstant
    <|> fmap SubroutineCall' subroutineCall
    <|> do
      name <- identifier
      Parser.symbol "["
      ex <- expression
      Parser.symbol "]"
      return $ VarNameExpression name ex
    <|> fmap VarName' identifier
    <|> do
      Parser.symbol "("
      ex <- expression
      Parser.symbol ")"
      return $ Expression' ex
    <|> do
      op <- unaryOp
      tm <- term
      return $ UnaryOp op tm

data SubroutineCall
  = SubroutineCall
      { subroutineName :: SubroutineName,
        expressions :: [Expression]
      }
  | MethodCall
      { classOrVarname :: Identifier,
        subroutineName :: SubroutineName,
        expressions :: [Expression]
      }
  deriving (Show)

subroutineCall :: Parser SubroutineCall
subroutineCall =
  do
    do
      name <- identifier
      Parser.symbol "("
      exs <- expressionList
      Parser.symbol ")"
      return SubroutineCall {subroutineName = name, expressions = exs}
    <|> do
      name <- identifier
      Parser.symbol "."
      sname <- identifier
      Parser.symbol "("
      exs <- expressionList
      Parser.symbol ")"
      return MethodCall {classOrVarname = name, subroutineName = sname, expressions = exs}

type ExpressionList = [Expression]

expressionList :: Parser [Expression]
expressionList = do
  e <- expression
  es <- many $ do
    Parser.symbol ","
    e <- expression
    return e
  return (e : es)
  <|> return []

data Op = Plus | Minus | Product | Dividion | And | Or | Lt | Gt | Eq deriving (Show)

op :: Parser Op
op =
  mapping
    [ ("+", Plus),
      ("-", Minus),
      ("*", Product),
      ("/", Dividion),
      ("&", And),
      ("|", Or),
      ("<", Lt),
      (">", Gt),
      ("=", Eq)
    ]

data UnaryOp = Negative | Childa deriving (Show)

unaryOp :: Parser UnaryOp
unaryOp = mapping [("-", Negative), ("~", Childa)]

data KeywordConstant = True | False | Null | This deriving (Show)

keywordConstant :: Parser KeywordConstant
keywordConstant =
  mapping
    [ ("true", Expression.True),
      ("false", Expression.False),
      ("null", Expression.Null),
      ("this", Expression.This)
    ]

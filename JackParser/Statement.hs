module Statement where

import Control.Applicative
import Control.Monad (join)
import Expression
import Parser
import Parser qualified as Parse
import Tokens

type Statements = [Statement]

statements :: Parser Statements
statements = many statement

data Statement = Let LetStatement | If IfStatement | While WhileStatement | Do DoStatement | Return ReturnStatement deriving (Show)

statement :: Parser Statement
statement = fmap Let letStatement <|> fmap If ifStatement <|> fmap While whileStatement <|> fmap Do doStatement <|> fmap Return returnStatement

data LetStatement = LetStatement
  { varName :: VarName,
    letLeftExpression :: Maybe Expression,
    letRightExpression :: Expression
  }
  deriving (Show)

letStatement :: Parser LetStatement
letStatement = do
  Parser.symbol "let"
  vName <- identifier
  lEx <- optional $ do
    Parser.symbol "["
    ex <- expression
    Parser.symbol "]"
    return $ ex
  Parser.symbol "="
  rEx <- expression
  Parser.symbol ";"
  return LetStatement {varName = vName, letLeftExpression = lEx, letRightExpression = rEx}

data IfStatement = IfStatement
  { ifExpression :: Expression,
    thenStatements :: Statements,
    elseStatements :: Maybe Statements
  }
  deriving (Show)

ifStatement :: Parser IfStatement
ifStatement = do
  Parser.symbol "if"
  Parser.symbol "("
  ex <- expression
  Parser.symbol ")"
  Parser.symbol "{"
  thenStmt <- statements
  Parser.symbol "}"
  elsStmt <- optional $ do
    Parser.symbol "else"
    Parser.symbol "{"
    stmt <- statements
    Parser.symbol "}"
    return $ stmt
  Parser.symbol "}"
  return $ IfStatement {ifExpression = ex, thenStatements = thenStmt, elseStatements = elsStmt}

data WhileStatement = WhileStatement
  { whileExpression :: Expression,
    whileStatements :: Statements
  }
  deriving (Show)

whileStatement :: Parser WhileStatement
whileStatement = do
  Parser.symbol "while"
  Parser.symbol "("
  ex <- expression
  Parser.symbol ")"
  Parser.symbol "{"
  stmt <- statements
  Parser.symbol "}"
  return WhileStatement {whileExpression = ex, whileStatements = stmt}

data DoStatement = DoStatement SubroutineCall deriving (Show)

doStatement :: Parser DoStatement
doStatement = do
  Parser.symbol "do"
  sc <- subroutineCall
  Parser.symbol ";"
  return $ DoStatement sc

data ReturnStatement = ReturnStatement (Maybe Expression) deriving (Show)

returnStatement :: Parser ReturnStatement
returnStatement = do
  Parser.symbol "return"
  ex <- optional expression
  Parser.symbol ";"
  return $ ReturnStatement ex

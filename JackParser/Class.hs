module Class where

import Control.Applicative
import Parser
import Statement
import Text.Read qualified as Parser
import Tokens

data Class = Class
  { className :: ClassName,
    classVarDec' :: [ClassVarDec],
    subroutineDec' :: [SubroutineDec]
  }
  deriving (Show)

class' :: Parser Class
class' = do
  Parser.symbol "class"
  cn <- identifier
  Parser.symbol "{"
  cvd <- many classVarDec
  sd <- many subroutineDec
  Parser.symbol "}"
  return Class {className = cn, classVarDec' = cvd, subroutineDec' = sd}

data ClassVarAccessor = Static | Field deriving (Show)

classVarAccessor :: Parser ClassVarAccessor
classVarAccessor = mapping [("static", Static), ("field", Field)]

data ClassVarDec = ClassVarDec
  { accessor :: ClassVarAccessor,
    classVarType :: Type,
    classVarName :: VarName,
    classVarNames :: [VarName]
  }
  deriving (Show)

classVarDec :: Parser ClassVarDec
classVarDec = do
  ac <- classVarAccessor
  t <- type'
  vn <- identifier
  vns <- many $ do
    Parser.symbol ","
    vn <- identifier
    return vn
  return ClassVarDec {accessor = ac, classVarType = t, classVarName = vn, classVarNames = vns}

data Type = Int' | Char' | Boolean' | ClassName' ClassName deriving (Show)

type' :: Parser Type
type' =
  do
    mapping [("int", Int'), ("char", Char'), ("boolean", Boolean')]
    <|> do
      id <- Tokens.identifier
      return $ ClassName' id

data SubroutineType = Contructor | Function | Method deriving (Show)

subroutineType :: Parser SubroutineType
subroutineType = mapping [("constructor", Contructor), ("function", Function), ("method", Method)]

data ReturnType = T' Type | Void deriving (Show)

returnType :: Parser ReturnType
returnType =
  do
    Parser.symbol "void"
    return Void
    <|> do
      t <- type'
      return $ T' t

data SubroutineDec = SubroutineDec
  { stype :: SubroutineType,
    returnType' :: ReturnType,
    subroutineName :: SubroutineName,
    paramerters :: [Parameter],
    subroutineBody' :: SubroutineBody
  }
  deriving (Show)

subroutineDec :: Parser SubroutineDec
subroutineDec = do
  st <- subroutineType
  rt <- returnType
  sn <- Tokens.identifier
  Parser.symbol "("
  pl <- parameterList
  Parser.symbol ")"
  sb <- subroutineBody
  return SubroutineDec {stype = st, returnType' = rt, subroutineName = sn, paramerters = pl, subroutineBody' = sb}

data Parameter = Parameter
  { parameterType :: Type,
    name :: VarName
  }
  deriving (Show)

parameter :: Parser Parameter
parameter =
  do
    t <- type'
    v <- Tokens.identifier
    return Parameter {parameterType = t, name = v}

type ParameterList = [Parameter]

parameterList :: Parser ParameterList
parameterList =
  do
    p <- parameter
    ps <- many $ do
      Parser.symbol ","
      p <- parameter
      return p
    return (p : ps)
    <|> return []

data SubroutineBody = SubroutineBody
  { varDec :: [VarDec],
    statements :: Statements
  }
  deriving (Show)

subroutineBody :: Parser SubroutineBody
subroutineBody = do
  Parser.symbol "{"
  vd <- many varDec'
  stmt <- Statement.statements
  Parser.symbol "}"
  return SubroutineBody {varDec = vd, Class.statements = stmt}

data VarDec = VarDec
  { varType :: Type,
    varName :: VarName,
    varNames :: [VarName]
  }
  deriving (Show)

varDec' :: Parser VarDec
varDec' = do
  Parser.symbol "var"
  t <- type'
  v <- Tokens.identifier
  vs <- many $ do
    Parser.symbol ","
    vn <- Tokens.identifier
    return vn
  Parser.symbol ";"
  return VarDec {varType = t, varNames = vs, Class.varName = v}

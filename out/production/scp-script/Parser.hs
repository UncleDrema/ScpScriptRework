{-# LANGUAGE BlockArguments #-}

module Parser where

import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import AST

op :: Parser String
op = do
  whitespace
  o <- operator
  whitespace
  return o

binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

opList arity = opList'
	where
    opList' [op] = [arity op Ex.AssocLeft]
    opList' (op:ops) = arity op Ex.AssocLeft : opList' ops

binList = opList binary

binops = [
  binList ["*", "/", "//", "%"]
  , binList ["+", "-"]
  , binList ["<", "=", "<=", ">=", "==", "!="]
	]

expr :: Parser Expr
expr = Ex.buildExpressionParser binops factor

factor :: Parser Expr
factor = try block
      <|> try function
      <|> try int
      <|> try call
      <|> try definition
      <|> try variable
      <|> try ifelse
      <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
  def <- function
  reservedOp ";"
  return def

exprType :: Parser ExprType
exprType =
  try ( do
    typeID <- identifier
    return $ case typeID of
      "int" -> IntType
      "float" -> FloatType
      "void" -> VoidType
      "bool" -> BooleanType)
  <|> try ( parens $ do
    fromTypes <- commaSep exprType
    reserved "->"
    CallableType fromTypes <$> exprType )

int :: Parser Expr
int = Int <$> integer

variable :: Parser Expr
variable = Var <$> identifier

definition :: Parser Expr
definition = do
  varType <- exprType
  whitespace
  Def varType <$> identifier

codeBlock :: Parser [Expr]
codeBlock = braces $ many $
  do e <- expr
     reserved ";"
     return e

block :: Parser Expr
block = Block <$> codeBlock

function :: Parser Expr
function = do
  funcType <- exprType
  name <- identifier
  args <- parens $ commaSep definition
  mReturns <- optionMaybe $ do
    reserved "returns"
    id <- identifier
    return id
  body <- do
    reserved "="
    block' <- optionMaybe codeBlock
    case block' of
      Just codeBlock' -> return codeBlock'
      Nothing -> do
        e <- expr
        return [e]
  return $ Function funcType name args mReturns body
  
call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

ifelse :: Parser Expr
ifelse = do
  reserved "if"
  cond <- parens expr
  tr <- codeBlock
  fl <- optionMaybe $ do
    reserved "else"
    code <- codeBlock
    return code
  return $ If cond tr (fromMaybe [] fl)
  
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseCode :: String -> Either ParseError AST
parseCode = parse (contents toplevel) "<stdin>"
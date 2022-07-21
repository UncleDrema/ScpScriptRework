{-# LANGUAGE BlockArguments #-}

module Parser where

import Data.Maybe
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.String (Parser)

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

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s = Ex.Infix (reservedOp s >> return (BinaryOp s))

opList :: (t -> Ex.Assoc -> a) -> [t] -> [a]
opList arity = opList'
  where
    opList' []           = []
    opList' [oper]       = [arity oper Ex.AssocLeft]
    opList' (oper:opers) = arity oper Ex.AssocLeft : opList' opers

binList :: [String] -> [Ex.Operator String () Identity Expr]
binList = opList binary

binops :: [[Ex.Operator String () Identity Expr]]
binops = [
    binList ["*", "/", "//", "%"]
  , binList ["+", "-"]
  , binList ["<", "=", "<=", ">=", "==", "!="]
  ]

expr :: Parser Expr
expr  = Ex.buildExpressionParser binops factor

factor :: Parser Expr
factor  = try block
      <|> try function
      <|> try funcReturn
      <|> try int
      <|> try float'
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
toplevel  = many $ do
  def    <- function
  reservedOp ";"
  return def

exprType :: Parser ExprType
exprType  =
  try ( do
    typeID <- identifier
    return $ case typeID of
      "int"   -> IntType
      "float" -> FloatType
      "void"  -> VoidType
      "bool"  -> BooleanType
      _       -> AutoType)
  <|> try ( parens $ do
    fromTypes <- commaSep exprType
    reserved "->"
    CallableType fromTypes <$> exprType )

int :: Parser Expr
int  = Int <$> integer

float' :: Parser Expr
float'  = Float <$> float

variable :: Parser Expr
variable  = Var <$> identifier

definition :: Parser Expr
definition  = do
  varType     <- exprType
  whitespace
  Def varType <$> identifier

codeBlock :: Parser [Expr]
codeBlock  = braces $ many
  do e <- expr
     reserved ";"
     return e

block :: Parser Expr
block  = Block <$> codeBlock

function :: Parser Expr
function  = do
  funcType <- exprType
  name     <- identifier
  args     <- parens $ commaSep definition
  body     <- do
    reserved "="
    block'   <- optionMaybe codeBlock
    case block' of
      Just codeBlock' -> return codeBlock'
      Nothing ->
        case funcType of
          VoidType -> do
             e <- expr
             return [e]
          _ -> do
             e <- funcReturn
             return [e]
  return $ Function funcType name args body

funcReturn :: Parser Expr
funcReturn  = do
  reserved "return"
  Return  <$> optionMaybe expr

call :: Parser Expr
call  = do
  name   <- identifier
  args   <- parens $ commaSep expr
  return $ Call name args

ifelse :: Parser Expr
ifelse  = do
  reserved "if"
  cond <- parens expr
  tr   <- codeBlock
  fl   <- optionMaybe $ do
    reserved "else"
    codeBlock
  return $ If cond tr (fromMaybe [] fl)

parseExpr :: String -> Either ParseError Expr
parseExpr  = parse (contents expr) "<stdin>"

parseCode :: String -> Either ParseError AST
parseCode  = parse (contents toplevel) "<stdin>"
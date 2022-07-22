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
    binList ["*", "/", "%"]
  , binList ["+", "-"]
  , binList ["<", ">", "=", "<=", ">=", "==", "!="]
  , binList ["+=", "-=", "*=", "/="]
  ]

expr :: Parser Expr
expr  = Ex.buildExpressionParser binops factor
    

factor :: Parser Expr
factor  =  try block
       <|> try function
       <|> try funcReturn
       <|> try int
       <|> try float'
       <|> try string'
       <|> try call
       <|> try (definition exprType)
       <|> try variable
       <|> try ifelse
       <|> parens expr
       <|> true
       <|> false
       <|> while

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel  = many $ do
  function

exprType :: Parser ExprType
exprType
   =  try funcTypes
  <|> try voidT
  <|> try varT
  
funcTypes :: Parser ExprType
funcTypes
   =  try funcT
  <|> try floatT
  <|> try intT
  <|> try boolT

funcT :: Parser ExprType
funcT = parens $ do
    fromTypes <- commaSep exprType
    reserved "->"
    CallableType fromTypes <$> exprType

intT :: Parser ExprType
intT = do
  _ <- reserved "int"
  return IntType
  
varT :: Parser ExprType
varT = do
  _ <- reserved "var"
  return AutoType

floatT :: Parser ExprType
floatT = do
  _ <- reserved "float"
  return FloatType

voidT :: Parser ExprType
voidT = do
  _ <- reserved "void"
  return VoidType

boolT :: Parser ExprType
boolT = do
  _ <- reserved "bool"
  return BooleanType

int :: Parser Expr
int  = Int <$> integer

float' :: Parser Expr
float'  = Float <$> float

true :: Parser Expr
true = do
  reserved "true"
  return $ Bool True
  
false :: Parser Expr
false = do
  reserved "false"
  return $ Bool False

string' :: Parser Expr
string'  = String <$> Lexer.string

variable :: Parser Expr
variable  = Var <$> identifier

definition :: Parser ExprType -> Parser Expr
definition t = do
  varType <- t
  whitespace
  Def varType <$> identifier

codeBlock :: Parser [FinalExpr]
codeBlock  = braces $ many
  do e <- expr
     reserved (ending e)
     return $ FE e

block :: Parser Expr
block  = Block <$> codeBlock

function :: Parser Expr
function  = do
  funcType' <- optionMaybe exprType
  let funcType = fromMaybe VoidType funcType'
  name     <- identifier
  args     <- parens $ commaSep (definition funcTypes)
  body     <- do
    reserved "="
    block'   <- optionMaybe block
    case block' of
      Just codeBlock' -> return codeBlock'
      Nothing ->
        case funcType of
          VoidType -> do
             e <- expr
             reserved ";"
             return (Block [FE e])
          _ -> do
             e <- funcReturn
             reserved ";"
             return (Block [FE e])
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
  tr   <- expr
  fl   <- optionMaybe $ do
    reserved "else"
    try expr
    <|> try ifelse
  return $ If cond tr (fromMaybe (Block []) fl)
  
while :: Parser Expr
while = do
  reserved "while"
  cond <- parens expr
  block' <- expr
  return $ While cond block'

parseExpr :: String -> Either ParseError Expr
parseExpr  = parse (contents expr) "<stdin>"

parseCode :: String -> Either ParseError AST
parseCode  = parse (contents toplevel) "<stdin>"
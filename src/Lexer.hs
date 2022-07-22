module Lexer where

import Text.Parsec (many)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops               = ["*", "-", "+", "/", "=", "==", "+=", "-=", "*=", "/=", "."]
    names             = types ++ keywords ++ punctuation
    style             = emptyDef {
      Tok.commentLine     = "//"
    , Tok.commentStart    = "/*"
    , Tok.commentEnd      = "*/"
    , Tok.caseSensitive   = True
    , Tok.reservedNames   = names
    , Tok.reservedOpNames = ops
    }
    types = ["int", "void", "bool", "float"]
    keywords = ["if", "else", "return", "->", "while", "for", "true", "false", "var"]
    punctuation = [";", ",", "{", "}", "(", ")"]

integer     :: Parser Integer
integer      = Tok.integer lexer
float       :: Parser Double
float        = Tok.float lexer
identifier  :: Parser String
identifier   = Tok.identifier lexer
reservedOp  :: String -> Parser ()
reservedOp   = Tok.reservedOp lexer
parens      :: Parser a -> Parser a
parens       = Tok.parens lexer
braces      :: Parser a -> Parser a
braces       = Tok.braces lexer
commaSep    :: Parser a -> Parser [a]
commaSep     = Tok.commaSep lexer
semiSep     :: Parser a -> Parser [a]
semiSep      = Tok.semiSep lexer
whitespace  :: Parser ()
whitespace   = Tok.whiteSpace lexer
reserved    :: String -> Parser ()
reserved     = Tok.reserved lexer
string      :: Parser String
string       = Tok.stringLiteral lexer

sumParser :: Parser Integer
sumParser = do
  first      <- integer
  reservedOp "+"
  second     <- integer
  return     (first + second)

operator :: Parser String
operator = do
  c      <- Tok.opStart emptyDef
  cs     <- many $ Tok.opLetter emptyDef
  return (c:cs)
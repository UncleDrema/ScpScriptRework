module Lexer where

import Text.Parsec (many)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["*", "-", "*", "/", ";"]
    names = ["if", "else"]
    style = emptyDef {
      Tok.commentLine = "//"
    , Tok.commentStart = "/*"
    , Tok.commentEnd = "*/"
    , Tok.caseSensitive = True
    , Tok.reservedNames = names
    , Tok.reservedOpNames = ops
    }

integer :: Parser Integer
integer = Tok.integer lexer
float :: Parser Double
float = Tok.float lexer
identifier :: Parser String
identifier = Tok.identifier lexer
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
parens     = Tok.parens lexer
braces     = Tok.braces lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
whitespace = Tok.whiteSpace lexer
reserved   = Tok.reserved lexer

sumParser :: Parser Integer
sumParser = do
  first <- integer
  reservedOp "+"
  second <- integer
  return (first + second)

operator :: Parser String
operator = do
  c <- Tok.opStart emptyDef
  cs <- many $ Tok.opLetter emptyDef
  return (c:cs)
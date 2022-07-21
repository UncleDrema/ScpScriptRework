module Lib
  (
  sumParser
  ) where

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["*", "-", "*", "/"]
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

sumParser :: Parser Integer
sumParser = do
  first <- integer
  reservedOp "+"
  second <- integer
  return (first + second)
  
parseSum :: String -> Either ParseError Integer
parseSum = parse sumParser ""

getParseSum :: Parser Integer -> (String -> Either ParseError Integer)
getParseSum intParser = parse intParser ""
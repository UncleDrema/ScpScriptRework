module ParseUtils
  (
    ErrMsg(..),
    unErr,
    unwrap,
    unwrapped,
    makeParse,
    runParser
  )
  where

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
  
newtype ErrMsg = ErrMsg String
unErr :: ErrMsg -> String
unErr (ErrMsg s) = s

unwrap :: ErrMsg -> Either ParseError a -> a
unwrap err x = case x of
  Right r -> r
  Left _ -> error $ unErr err

unwrapped :: ErrMsg -> (String -> Either ParseError a) -> String -> a
unwrapped err parser = unwrap err . parser

makeParse :: String -> Parser a -> (String -> Either ParseError a)
makeParse source parser = parse parser source

runParser :: String -> ErrMsg -> Parser a -> String -> a
runParser source err parser = unwrapped err (makeParse source parser)
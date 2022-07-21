{-# LANGUAGE RecordWildCards #-}

module ParserTest
  (
    genParserTest
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec.String (Parser)
import Text.Parsec (parse, ParseError)

newtype ErrMsg = ErrMsg String
unErr :: ErrMsg -> String
unErr (ErrMsg s) = s

unwrap :: ErrMsg -> Either ParseError a -> a
unwrap err x = case x of
  Right r -> r
  Left _ -> error $ unErr err

makeParse :: Parser a -> (String -> Either ParseError a)
makeParse parser = parse parser "Test"

unwrapped :: ErrMsg -> (String -> Either ParseError a) -> String -> a
unwrapped err parser = unwrap err . parser

data TestBase a = TestBase
    { parser :: Parser a
    , err :: ErrMsg
    }
    
data TestParams a = TestParams
    { input :: String
    , expect :: a
    }

data TestSettings a = TestSettings
    { base :: TestBase a
    , params :: TestParams a
    }

genParserTest :: (Eq a, Show a) => Parser a -> String -> String -> a -> TestTree
genParserTest parser' err' input' expect' = genParserTest' TestSettings {
      base = TestBase {
            parser = parser'
          , err = ErrMsg err'
          }
    , params = TestParams {
            input = input'
          , expect = expect'          
          }
    }


genParserTest' :: (Eq a, Show a) => TestSettings a -> TestTree
genParserTest' TestSettings{base = TestBase{..}, params = TestParams{..}} =
    testCase ("Parsing \"" <> input <> "\"" <> " equals \"" <> show expect <> "\"") $ res @?= expect
      where
        res = unwrapped err (makeParse parser) input

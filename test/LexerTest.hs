{-# LANGUAGE RecordWildCards #-}

module LexerTest
  (
    genParserTest
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Parsec.String (Parser)
import ParseUtils (ErrMsg(..), unwrapped, makeParse)

data TestBase a = TestBase
    { parser :: Parser a
    , err    :: ErrMsg
    }

data TestParams a = TestParams
    { input  :: String
    , expect :: a
    }

data TestSettings a = TestSettings
    { base   :: TestBase a
    , params :: TestParams a
    }

genParserTest :: (Eq a, Show a) => Parser a -> String -> String -> a -> TestTree
genParserTest parser' err' input' expect' = genParserTest' TestSettings {
      base = TestBase {
            parser = parser'
          , err    = ErrMsg err'
          }
    , params = TestParams {
            input  = input'
          , expect = expect'
          }
    }


genParserTest' :: (Eq a, Show a) => TestSettings a -> TestTree
genParserTest' TestSettings{base = TestBase{..}, params = TestParams{..}} =
    testCase ("Lexing \"" <> input <> "\"" <> " equals \"" <> show expect <> "\"") $ res @?= expect
      where
        res = unwrapped err (makeParse "Test" parser) input

import Test.Tasty (defaultMain, testGroup, TestTree)
import Lib (sumParser)
import ParserTest (genParserTest)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [parseTests]

  
sumTest :: String -> Integer -> TestTree
sumTest = genParserTest sumParser "Sum parsing error!"

parseTests :: TestTree
parseTests = testGroup "Parsing tests"
    [ sumTest "5 +3" 8
    , sumTest "-5+ 5" 0
    , sumTest "0+0" 0
    , sumTest "-1 + -2" (-3)
    ]
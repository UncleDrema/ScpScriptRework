import Test.Tasty (defaultMain, testGroup, TestTree)
import Lib (sumParser)
import ParserTest (genParserTest)
import Test.Tasty.HUnit (testCase, (@?=))
import Utils (join)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ parseTests
    , utilsTests
    ]

  
sumTest :: String -> Integer -> TestTree
sumTest = genParserTest sumParser "Sum parsing error!"

utilsTests :: TestTree
utilsTests = testGroup "Utils tests"
    [
      testCase "Join for comma and space separated data" $ join ", " ["a", "b", "c"] @?= "a, b, c"
    , testCase "Join for empty separator" $ join "" ["a", "b", "c"] @?= "abc"
    , testCase "Join for empty arguments" $ join ", " [] @?= ""
    ]

parseTests :: TestTree
parseTests = testGroup "Parsing tests"
    [ sumTest "5 +3" 8
    , sumTest "-5+ 5" 0
    , sumTest "0+0" 0
    , sumTest "-1 + -2" (-3)
    ]
import Test.Tasty (defaultMain, testGroup, TestTree)
import Lexer (sumParser)
import ParserTest (genParserTest)
import Test.Tasty.HUnit (testCase, (@?=))
import StringUtils (join, addToLast, smartJoin, joinOrSplit)

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
utilsTests = testGroup "StringUtils tests"
    [
      testGroup "join tests" [
          testCase "Join for comma and space separated data" $ join ", " ["a", "b", "c"] @?= "a, b, c"
        , testCase "Join for empty separator" $ join "" ["a", "b", "c"] @?= "abc"
        , testCase "Join for empty arguments" $ join ", " [] @?= ""
        ]
    , testGroup "addToLast tests" [
          testCase "Adding to last" $ addToLast ["hey", "ho"] "ly" @?= ["hey", "holy"]
        , testCase "Adding to last empty list" $ addToLast [] "wow" @?= ["wow"]
        , testCase "Adding empty to last" $ addToLast ["hey", "hello"] "" @?= ["hey", "hello"]
        ]
    , testGroup "smartJoin tests" [
          testCase "joining len <40 no white" $ smartJoin ["hey", "hello", "what?"] @?= ["hey hello what?"]
        , testCase "joining len <40 has white" $ smartJoin ["hey", "  ", "hello", "", "what?"] @?= ["hey hello what?"]
        , testCase "joining len > 40 no white"
            $ smartJoin [replicate 20 'f', "hey", replicate 30 'g'] @?= [replicate 20 'f', "hey", replicate 30 'g']
        , testCase "joining len > 40 no white"
            $ smartJoin [replicate 20 'f', "  ", "hey", replicate 30 'g'] @?= [replicate 20 'f', "  ", "hey", replicate 30 'g']
        ]
    ]

parseTests :: TestTree
parseTests = testGroup "Parsing tests"
    [ sumTest "5 +3" 8
    , sumTest "-5+ 5" 0
    , sumTest "0+0" 0
    , sumTest "-1 + -2" (-3)
    ]
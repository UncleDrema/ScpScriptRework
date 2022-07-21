import Test.Tasty (defaultMain, testGroup, TestTree)
import Lexer
import LexerTest (genParserTest)
import Test.Tasty.HUnit (testCase, (@?=))
import StringUtils
import Pretty (Pretty(..), ByShow(..))

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests  =
  testGroup
    "Unit tests"
    [ lexTests
    , utilsTests
    , prettyTests
    ]

utilsTests :: TestTree
utilsTests  = testGroup "StringUtils tests"
    [
      testGroup "join tests" [
          testCase "Join for comma and space separated data" $ joinComma ["a", "b", "c"]  @?= "a, b, c"
        , testCase "Join for empty separator"                $ join "" ["a", "b", "c"]    @?= "abc"
        , testCase "Join for empty arguments"                $ joinComma []               @?= ""
        , testCase "Join with spaces"                        $ joinSpaces ["a", "b", "c"] @?= "a b c"
        , testCase "Join with newline"                       $ joinLines ["a", "b", "c"]  @?= "a\nb\nc"
        ]
    , testGroup "addToLast tests" [
          testCase "Adding to last"            $ addToLast ["hey", "ho"] "ly"  @?= ["hey", "holy"]
        , testCase "Adding to last empty list" $ addToLast [] "wow"            @?= ["wow"]
        , testCase "Adding empty to last"      $ addToLast ["hey", "hello"] "" @?= ["hey", "hello"]
        , testCase "Adding to one element"     $ addToLast ["hel"] "lo"         @?= ["hello"]
        ]
    , testGroup "smartJoin tests" [
          testCase "joining len <40 no white"  $ smartJoin ["hey", "hello", "what?"]           @?= ["hey hello what?"]
        , testCase "joining len <40 has white" $ smartJoin ["hey", "  ", "hello", "", "what?"] @?= ["hey hello what?"]
        , testCase "joining len > 40 no white"
            $ smartJoin [replicate 20 'f', "hey", replicate 30 'g']       @?= [replicate 20 'f', "hey", replicate 30 'g']
        , testCase "joining len > 40 no white"
            $ smartJoin [replicate 20 'f', "  ", "hey", replicate 30 'g'] @?= [replicate 20 'f', "  ", "hey", replicate 30 'g']
        ]
    ]
    
sumTest :: String -> Integer -> TestTree
sumTest  = genParserTest sumParser "Sum lexing error!"

floatTest :: String -> Double -> TestTree
floatTest  = genParserTest float "Float lexing error!"

idTest :: String -> String -> TestTree
idTest  = genParserTest identifier "Identifier lexing error!"

whiteTest  :: String -> TestTree
whiteTest s = genParserTest whitespace "Whitespace lexing error!" s ()

operatorTest :: String -> String -> TestTree
operatorTest = genParserTest operator "Operator lexing error!"

lexTests :: TestTree
lexTests  = testGroup "Lexing tests"
    [ testGroup "Sum tests"
        [ sumTest "5 +3"    8
        , sumTest "-5+ 5"   0
        , sumTest "0+0"     0
        , sumTest "-1 + -2" (-3)
        ]
    , testGroup "Float tests"
        [ floatTest "5.2" 5.2
        , floatTest "0.0f" 0.0
        , floatTest "1e3" 1000
        , floatTest "2e-3" 0.002
        ]
    , testGroup "Identifier tests"
        [ idTest "some" "some"
        , idTest "_hey" "_hey"
        , idTest "a32" "a32"
        ]
    , testGroup "Whitespace tests"
        [ whiteTest ""
        , whiteTest "  "
        , whiteTest "\n "
        , whiteTest "  \t "
        ]
    , testGroup "Operator tests"
        [ operatorTest "<= " "<="
        , operatorTest ">=>" ">=>"
        , operatorTest "+-  " "+-"
        , operatorTest "?!f" "?!"
        ]
    ]

prettyTests :: TestTree
prettyTests  = testGroup "Pretty tests"
    [ testCase "Just string"  $ prettify (BS "Hey") @?= ["Hey"]
    , testCase "Empty string" $ prettify (BS "") @?= [""]
    , testCase "Depth one pretty"   $ prettify (map BS ["ab", "bc", "cd"]) @?= [" ab", " bc", " cd"]
    , testCase "Depth two pretty"   $ prettify [map BS ["ab", "bc", "cd"]] @?= ["  ab", "  bc", "  cd"]
    ]
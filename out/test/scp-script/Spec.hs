import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Lib (double, half, nothing)

main :: IO ()
main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    (doublingMakesNumbersBigger
    ++ halvingMakesNumbersSmaller 
    ++ nothingIsNothing)

doublingMakesNumbersBigger =
  [testCase "Double of 4 is 8" $ assertEqual [] 8 (double 4)]

halvingMakesNumbersSmaller =
  [testCase "Half of 9 is 4" $ assertEqual [] 4 (half 9)]
  
nothingIsNothing =
  [ testCase "Not changed 5" $ assertEqual [] 5 (nothing 5)
  , testCase "Not changed 8" $ assertEqual [] 8 (nothing 8)
  ]
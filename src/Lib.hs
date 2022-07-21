module Lib
  ( double
  , half
  , someFunc
  , nothing
  ) where

compose :: (x -> y) -> (y -> z) -> x -> z
compose f g x =
  g (f x)


double :: Num a => a -> a
double = (* 2)

half :: Integral a => a -> a
half x = x `div` 2

nothing :: Integer -> Integer
nothing = compose double half

someFunc :: IO ()
someFunc = putStrLn "someFunc"

module Lib where
      
double :: Num a => a -> a
double x = x * 2

half :: Integral a => a -> a
half x = div x 2

someFunc :: IO ()
someFunc = putStrLn "someFunc"

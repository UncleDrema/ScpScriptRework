module Utils
  (
    join
  ) where

join :: String -> [String] -> String
join _ [] = ""
join a xs = foldr1 concat' xs
  where
    concat' b c = b ++ a ++ c
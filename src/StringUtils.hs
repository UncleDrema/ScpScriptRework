{-# LANGUAGE ViewPatterns #-}

module StringUtils where

import Pretty (Pretty(..))
import Data.Char (isSpace)

join :: String -> [String] -> String
join _ [] = ""
join a xs = foldr1 concat' xs
  where
    concat' b c = b ++ a ++ c

joinArgs   :: [String] -> String
joinArgs    = join ", "
joinLines  :: [String] -> String
joinLines   = join "\n"
joinSpaces :: [String] -> String
joinSpaces  = join " "

addToLast :: [String] -> String -> [String]
addToLast strs []               = strs
addToLast [] str                = [str]
addToLast [s] str               = [s ++ str]
addToLast (reverse -> s:ss) str = reverse ss ++ [s ++ str]
addToLast strs str              = init strs ++ [last strs ++ str]

smartJoin :: [String] -> [String]
smartJoin strs = if sum (map length strs) < 40
  then [joinSpaces (filter (not . null) $ map (dropWhile isSpace) strs)]
  else strs

-- idk how that works, so no tests now
joinOrSplit :: Pretty a => [String] -> a -> [String]
joinOrSplit s e = case prettify e of
  [r]   -> addToLast s (" (" ++ r ++ ")")
  listR -> addToLast s " (" ++ ["  " ++ r' | r' <- listR] ++ [")"]
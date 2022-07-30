{-# LANGUAGE ViewPatterns #-}

module StringUtils where

import Pretty (Pretty(..))
import Data.Char (isSpace)

join :: String -> [String] -> String
join _ [] = ""
join a xs = foldr1 concat' xs
  where
    concat' b c = b ++ a ++ c

joinComma  :: [String] -> String
joinComma   = join ", "
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

filterEmpty :: [String] -> [String]
filterEmpty strs = filter (not . null) $ map (dropWhile isSpace) strs

-- ident list of something
joinOrSplit :: Pretty a => [String] -> a -> [String]
joinOrSplit s e = 
  let ident = case s of
                [] -> ""
                [""] -> ""
                _ -> " "
    in case prettify e of
      [r]   -> addToLast s (ident ++ "(" ++ r ++ ")")
      listR -> addToLast s (ident ++ "(") ++ listR ++ [")"]

module Main where

import Parser
import AST
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> putStrLn "Provide file name!"
    [filename] -> do
      code     <- readFile filename
      case parseCode code of
        Left err  -> print err
        Right ast -> putStrLn (joinedPrettyAST $ processAst ast)
    _          -> putStrLn "Provide one file name!"

processAst :: AST -> AST
processAst ast = mapMaybe getDecl ast ++ [ExecuteBuiltin (map FE ast)]

getDecl :: Expr -> Maybe Expr
getDecl (Function t name args _) = Just (TopDecl t name args)
getDecl _ = Nothing
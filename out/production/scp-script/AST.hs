{-# LANGUAGE TypeSynonymInstances #-}

module AST where

import StringUtils
import Pretty

type Name = String
type CodeBlock term = [term]
type AST = [Expr]

data ExprType
    = IntType
    | FloatType
    | VoidType
    | BooleanType
    | StringType
    | CallableType [ExprType] ExprType
    deriving (Eq)

instance Show ExprType where
  show IntType = "int"
  show FloatType = "float"
  show VoidType = "void"
  show BooleanType = "bool"
  show StringType = "string"
  show (CallableType args ret) = "(" ++ argsRepr ++ " -> " ++ show ret ++ ")"
    where
      argsRepr = case args of
        [] -> "()"
        args' -> joinComma (map show args')

data Expr
    = Int Integer
    | Float Double
    | Var Name
    | String String
    | Def ExprType Name
    | Block (CodeBlock Expr)
    | Call Name [Expr]
    | Function ExprType Name [Expr] (CodeBlock Expr)
    | BinaryOp String Expr Expr
    | Return (Maybe Expr)
    | If Expr (CodeBlock Expr) (CodeBlock Expr)
    | Unsafe [String]
    deriving (Eq, Show)

prettifyAST :: Pretty e => [e] -> [String]
prettifyAST  = map (joinLines . prettify)

joinedPrettyAST :: Pretty e => [e] -> String
joinedPrettyAST  = joinLines . prettifyAST

--  Pretty big
instance Pretty Expr where
    prettify expr = case expr of
        (Int i)                       -> [joinSpaces ["Int", show i]]
        (Float f)                     -> [joinSpaces ["Float", show f]]
        (String s)                    -> [joinSpaces ["String", show s]]
        (Var name)                    -> [joinSpaces ["Var", name]]
        (Unsafe unString)             -> [joinSpaces ["Unsafe", show (unlines unString)]]
        (Def exprType name)           -> [joinSpaces ["Def", show exprType, name]]
        (Block codeBlock)             -> smartJoin ("Block {" : prettify codeBlock ++ ["}"])
        (Call name exprs)             -> smartJoin (joinSpaces ["Call", name, "("] : prettify exprs ++ [")"])
        (Function t name args body)   -> header : prettify body ++ ["}"]
          where
            header = joinSpaces ["Function ", show name, show t, "; args", show args, "{"]
        (Return e)                    -> [joinSpaces ["Return", show e]]
        (BinaryOp op expr1 expr2)     -> joinOrSplit (joinOrSplit ["BinaryOp " ++ op] expr1) expr2
        (If cond thenBlock elseBlock) -> header ++ blocks
          where
            header = addToLast (joinOrSplit ["If"] cond) " {"
            blocks = prettify thenBlock ++ ["}", "else {"] ++ prettify elseBlock ++ ["}"]
--}

{-- Pretty C-like
instance Pretty Expr where
    prettify expr = case expr of
        (Int i)                       -> [joinSpaces [show i]]
        (Float f)                     -> [joinSpaces [show f]]
        (Var name)                    -> [joinSpaces [name]]
        (Def exprType name)           -> [joinSpaces [show exprType, name]]
        (Block codeBlock)             -> smartJoin ("{" : prettify codeBlock ++ ["}"])
        (Call name exprs)             -> smartJoin ([name, "("] ++ prettify exprs ++ [")"])
        (Function t name args body)   -> header : prettify body ++ ["}"]
          where
            header = joinSpaces ([show t, show name] ++ ["("] ++ prettify args ++ ["){"])
        (Return e)                    -> [joinSpaces ("return" : prettify e)]
        (BinaryOp op expr1 expr2)     -> joinOrSplit (joinOrSplit [] expr1 ++ [op]) expr2
        (If cond thenBlock elseBlock) -> header ++ blocks 
          where
            header = addToLast (joinOrSplit ["if"] cond) " {"
            blocks = prettify thenBlock ++ ["}", "else {"] ++ prettify elseBlock ++ ["}"]
--}
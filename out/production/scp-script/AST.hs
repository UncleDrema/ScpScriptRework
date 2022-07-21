{-# LANGUAGE TypeSynonymInstances #-}

module AST
  () where

import Data.Tree hiding (Tree(..))
import StringUtils
import Pretty (Pretty(..))

type Name = String
type CodeBlock term = [term]
type AST = [Expr]

data ExprType
    = IntType
    | FloatType
    | VoidType
    | BooleanType
    | CallableType [ExprType] ExprType
    deriving (Eq)

instance Show ExprType where
  show IntType = "int"
  show FloatType = "float"
  show VoidType = "void"
  show BooleanType = "bool"
  show (CallableType args ret) = "(" ++ argsRepr ++ " -> " ++ show ret ++ ")"
    where
      argsRepr = case args of
        [] -> "()"
        args' -> joinArgs (map show args')

data Expr
    = Int Integer
    | Float Double
    | Var Name
    | Def ExprType Name
    | Block (CodeBlock Expr)
    | Call Name [Expr]
    | Function ExprType Name [Expr] (Maybe Name) (CodeBlock Expr)
    | BinaryOp String Expr Expr
    | If Expr (CodeBlock Expr) (CodeBlock Expr)
    deriving (Eq, Show)

instance Pretty term => Pretty (CodeBlock term) where
    prettify terms = concatMap tabTerm terms
      where tabTerm t = map (" " ++) (prettify t)

instance Pretty Expr where
    prettify expr = case expr of
        (Int i) -> [joinSpaces ["Int", show i]]
        (Float f) -> [joinSpaces ["Float", show f]]
        (Var name) -> [joinSpaces ["Var", name]]
        (Def exprType name) -> [joinSpaces ["Def", show exprType, name]]
        (Block codeBlock) -> smartJoin ("Block {" : prettify codeBlock ++ ["}"])
        (Call name exprs) -> smartJoin (joinSpaces ["Call", name, "("] : prettify exprs ++ [")"])
        (Function t name args ret body) ->
            joinSpaces ["Function ", show name, show t, "; args", show args, "; returns", show ret, "{"]
            : prettify body ++ ["}"]
        (BinaryOp op expr1 expr2) -> joinOrSplit (joinOrSplit ["BinaryOp " ++ op] expr1) expr2
        (If cond thenBlock elseBlock) -> addToLast (joinOrSplit ["If"] cond) " {" ++ prettify thenBlock ++ ["}", "else {"] ++ prettify elseBlock ++ ["}"]

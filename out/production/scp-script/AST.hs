module AST
  () where

import Data.Tree hiding (Tree(..))
import Utils (join)

joinArgs = join ", "

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


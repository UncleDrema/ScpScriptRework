{-# LANGUAGE TypeSynonymInstances #-}

module AST where

import StringUtils
import Pretty

type Name = String
type AST = [Expr]

data ExprType
    = IntType
    | FloatType
    | VoidType
    | BooleanType
    | StringType
    | AutoType
    | CallableType [ExprType] ExprType
    deriving (Eq)

canBeArgs :: ExprType -> Bool
canBeArgs VoidType = False
canBeArgs AutoType = False
canBeArgs _        = True

instance Show ExprType where
  show IntType = "int"
  show FloatType = "double"
  show VoidType = "void"
  show BooleanType = "bool"
  show StringType = "string"
  show AutoType = "var"
{-- default
  show (CallableType args ret) = "(" ++ argsRepr ++ " -> " ++ show ret ++ ")"
    where
      argsRepr = case args of
        [] -> "()"
        args' -> joinComma (map show args')
--}
-- C#
  show (CallableType args ret) = "Func<" ++ argsRepr ++ ", " ++ show ret ++ ">"
    where
      argsRepr = case args of
        [] -> "()"
        args' -> joinComma (map show args')
--}

data Expr
    = Int Integer
    | Float Double
    | Var Name
    | Bool Bool
    | String String
    | Def ExprType Name
    | Block [FinalExpr]
    | Call Name [Expr]
    | Function ExprType Name [Expr] Expr
    | BinaryOp String Expr Expr
    | Return (Maybe Expr)
    | If Expr Expr Expr
    | TopDecl ExprType Name [Expr]
    | While Expr Expr
    | Dot Expr Expr
    | Parens Expr
    | ExecuteBuiltin [FinalExpr]
    deriving (Eq)

ending :: Expr -> String
ending (If _ then' _) = if isBlock then' then "" else ";"
ending (While _ block') = if isBlock block' then "" else ";"
ending (Block _) = ""
ending _ = ";"

isBlock :: Expr -> Bool
isBlock (Block []) = False
isBlock (Block _) = True
isBlock _ = False

newtype FinalExpr = FE {unFE :: Expr} deriving (Eq)
instance Pretty FinalExpr where
  prettify fe = addToLast (prettify (unFE fe)) ";"

prettifyAST :: Pretty e => [e] -> [String]
prettifyAST  = filterEmpty . map (joinLines . prettify)

joinedPrettyAST :: Pretty e => [e] -> String
joinedPrettyAST  = joinLines . prettifyAST

-- Pretty C#
instance Pretty Expr where
    prettify expr = case expr of
        (ExecuteBuiltin exprs)        -> "public static void Execute()" : prettify (Block exprs)
        (Parens e)                    -> "(" : prettify e ++ [")"]
        (Dot from to)                 -> addToLast (prettify from) ("." ++ t) ++ ts
            where (t:ts) = prettify to
        (Int i)                       -> [show i]
        (Float f)                     -> [show f ++ "f"]
        (Var name)                    -> [name]
        (Bool b)                      -> [if b then "true" else "false"]
        (String s)                    -> [show s]
        (Def exprType name)           -> [joinSpaces [show exprType, name]]
        (Block codeBlock)             -> map (" " ++ ) ("{" : prettify codeBlock ++ ["}"])
        (Call name exprs)             -> [name ++ "(" ++ joinComma (map (joinSpaces . prettify) exprs) ++ ")"]
        (TopDecl t name args)         -> [joinSpaces ["public static", delegate <> args',  name <> ";"]]
          where
            delegate = case t of
              VoidType -> "Action"
              _ -> "Func"
            cleanArgs = filter (canBeArgs . getType') args
            args' = case t of
              VoidType -> case cleanArgs of
                [] -> ""
                _ -> "<" ++ joinComma (map type' cleanArgs) ++ ">"
              _ -> "<" ++ joinComma (map type' (cleanArgs ++ [Def t ""])) ++ ">"
            type' e = case e of
              (Def eType _) -> show eType
              _ -> ""
            getType' e = case e of
              (Def eType _) -> eType
              _ -> VoidType
        (Function _ name args body)   -> header : prettify body ++ ["}"]
          where
            header = name <> " = " <> "(" <> vars' <> ") => {"
            cleanArgs = filter (canBeArgs . getType') args
            vars' = joinComma (map name' cleanArgs)
            getType' e = case e of
              (Def eType _) -> eType
              _ -> VoidType
            name' e = case e of
              (Def _ name'') -> name''
              _ -> ""
        (Return e)                    -> [joinSpaces ("return" : prettify e)]
        (BinaryOp "." expr1 expr2)    -> addToLast (prettify expr1) ("." ++ s) ++ ss
          where (s:ss) = prettify expr2
        (BinaryOp "=" expr1 expr2)    -> joinOrSplit (addToLast (prettify expr1) " =") expr2
        (BinaryOp op expr1 expr2)    -> joinOrSplit (addToLast (prettify expr1) (" " ++ op)) expr2
        (If cond thenBlock elseBlock) -> header ++ blocks
          where
            header = addToLast (joinOrSplit ["if"] cond) " {"
            blocks = addToLast (addToLast (prettify thenBlock) (ending thenBlock) ++ ["}", "else"] ++ prettify elseBlock) (ending elseBlock)
        (While cond block) -> header ++ block'
          where
            header = joinOrSplit ["while"] cond
            block' = addToLast (prettify block) (ending block)
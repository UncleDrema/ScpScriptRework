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
    | AutoType
    | CallableType [ExprType] ExprType
    deriving (Eq)
    
canBeArgs :: ExprType -> Bool
canBeArgs VoidType = False
canBeArgs AutoType = False
canBeArgs _        = True

instance Show ExprType where
  show IntType = "int"
  show FloatType = "float"
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
    | Block (CodeBlock FinalExpr)
    | Call Name [Expr]
    | Function ExprType Name [Expr] Expr
    | BinaryOp String Expr Expr
    | Return (Maybe Expr)
    | If Expr Expr Expr
    | TopDecl ExprType Name [Expr]
    | While Expr Expr
    deriving (Eq, Show)
    
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
instance Show FinalExpr where
  show fe = show (unFE fe)
instance Pretty FinalExpr where
  prettify fe = addToLast (prettify (unFE fe)) ";"

prettifyAST :: Pretty e => [e] -> [String]
prettifyAST  = map (joinLines . prettify)

joinedPrettyAST :: Pretty e => [e] -> String
joinedPrettyAST  = joinLines . prettifyAST

{--  Pretty big
instance Pretty Expr where
    prettify expr = case expr of
        (Int i)                       -> [joinSpaces ["Int", show i]]
        (Float f)                     -> [joinSpaces ["Float", show f]]
        (String s)                    -> [joinSpaces ["String", show s]]
        (Var name)                    -> [joinSpaces ["Var", name]]
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

-- Pretty C#
instance Pretty Expr where
    prettify expr = case expr of
        (Int i)                       -> [show i]
        (Float f)                     -> [show f]
        (Var name)                    -> [name]
        (Bool b)                      -> [if b then "true" else "false"]
        (String s)                    -> [show s]
        (Def exprType name)           -> [joinSpaces [show exprType, name]]
        (Block codeBlock)             -> smartJoin ("{" : prettify codeBlock ++ ["}"])
        (Call name exprs)             -> smartJoin ([name, "("] ++ [joinComma (map (joinSpaces . prettify) exprs)] ++ [")"])
        (TopDecl t name args)         -> header : [body, "});"]
          where
            header = "var " <> name <> " = new " <> delegate <> args' <> "((" <> vars' <> ") => {"
            (delegate, body) = case t of
              VoidType -> ("Action", "")
              _ -> ("Func", "return default;")
            cleanArgs = filter (canBeArgs . getType') args
            args' = case t of
              VoidType -> case cleanArgs of
                [] -> ""
                _ -> joinComma (map show cleanArgs)
              _ -> "<" ++ joinComma (map type' (cleanArgs ++ [Def t ""])) ++ ">"
            vars' = joinComma (map name' cleanArgs)
            type' e = case e of
              (Def eType _) -> show eType
              _ -> ""
            getType' e = case e of
              (Def eType _) -> eType
              _ -> VoidType
            name' e = case e of
              (Def _ name) -> name
              _ -> ""
        (Function t name args body)   -> header : prettify body ++ ["});"]
          where
            header = name <> " = " <> args' <> "((" <> vars' <> ") => {"
            cleanArgs = filter (canBeArgs . getType') args
            args' = case t of
              VoidType -> case cleanArgs of
                [] -> ""
                _ -> joinComma (map show cleanArgs)
              _ -> "<" ++ joinComma (map type' (cleanArgs ++ [Def t ""])) ++ ">"
            vars' = joinComma (map name' cleanArgs)
            type' e = case e of
              (Def eType _) -> show eType
              _ -> ""
            getType' e = case e of
              (Def eType _) -> eType
              _ -> VoidType
            name' e = case e of
              (Def _ name'') -> name''
              _ -> ""
        (Return e)                    -> [joinSpaces ("return" : prettify e)]
        (BinaryOp "=" expr1 expr2)    -> joinOrSplit (addToLast (prettify expr1) " =") expr2
        (BinaryOp op expr1 expr2)    -> joinOrSplit (addToLast (prettify expr1) (" " ++ op)) expr2
        (If cond thenBlock elseBlock) -> header ++ blocks
          where
            header = addToLast (joinOrSplit ["if"] cond) " {"
            blocks = prettify thenBlock ++ ["}", "else"] ++ prettify elseBlock
        (While cond block) -> header ++ block'
          where
            header = addToLast (joinOrSplit ["while"] cond) " {"
            block' = prettify block ++ ["}"]
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
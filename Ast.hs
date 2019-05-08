{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ast where


import Text.Printf
{-
Языковые конструкции:
* Выражения (арифметические, логические)
* Переменные
* let-связывания
* Условный оператор If.
* Объявление и вызов функций
* Шаблоны
* Сопоставление с шаблонами в аргументе функции
* Case-конструкция.
* Спецификация типов данных (data).

Пример программы:

data EitherSI = Left Bool | Right Int | Middle

f (Left b) = if b then -1 else 1
f (Right n) = n + 2
f Middle = 42

g x = if x == 0 then Left False else if x > 10 then Right 50 else Middle

main = case g (f Middle) of { (Right n) -> n; (Left b) -> 0; Middle -> -1; }

-}

type Name = String

type Ast = [Decl]

data Decl = DataDecl Data | BindDecl Bind | TypeDecl Name Type
  deriving Show

data Type = Arrow Type Type | TInt | TBool | TData String
  deriving (Show, Eq)

data Data = Data Name [Constr]
  deriving Show

-- Constr {Constr's name} {Constr's params}
data Constr = Constr Name [Name]
  deriving Show

data Bind = Bind Name [Pat] Expr
  deriving Show

data Pat = PatVar Name | PatCtr Name [Pat] | PatLit Lit
  deriving Show

data Expr = Var Name
          | Lit Lit
          | App Expr Expr
          | If Expr Expr Expr
          | Case Expr [(Pat, Expr)]
          | Let Name Expr Expr
          | UnOp UnOperator Expr
          | BinOp BinOperator Expr Expr
  deriving Show

data Lit = ILit Integer | BLit Bool
  deriving (Show, Eq)

data ArithOperator = Pow | Mul | Div | Sum | Sub
  deriving Show

data CmpOperator = Eq | Neq | Le | Lt | Ge | Gt
  deriving Show

data LogicalOperator = And | Or
  deriving Show

data BinOperator = AO ArithOperator | CO CmpOperator | LO LogicalOperator
  deriving Show

data UnOperator = Neg | Minus
  deriving Show

class Pretty a where
    pretty :: a -> String

instance Pretty Ast where
    pretty [] = ""
    pretty [d] = pretty d ++ ";"
    pretty (d:ds) = pretty d ++ ";\n" ++ pretty ds

instance Pretty Decl where
    pretty (DataDecl d) = pretty d
    pretty (BindDecl d) = pretty d
    pretty (TypeDecl name t) = ":" ++ name ++ ": " ++ pretty t

instance Pretty Type where
    pretty TInt = "Int"
    pretty TBool = "Bool"
    pretty (TData n) = n
    pretty (Arrow t1 t2) = pretty t1 ++ " -> " ++ pretty t2

instance Pretty Bind where
    pretty (Bind name args expr) = name ++ (concatMap (\p -> " " ++ pretty p) args) ++ " = " ++ pretty expr

instance Pretty Data where
    pretty (Data name []) = "data " ++ name
    pretty (Data name ctrs) = "data " ++ name ++ " = " ++ (concatMap (\ctr -> " | " ++ pretty ctr) ctrs)

instance Pretty Constr where
    pretty (Constr name []) = name
    pretty (Constr name ns) = "(" ++ name ++ (concatMap (" " ++) ns) ++ ")"

instance Pretty Pat where
    pretty (PatVar n) = n
    pretty (PatLit l) = show l
    pretty (PatCtr ctr []) = ctr
    pretty (PatCtr ctr ps) = "(" ++ ctr ++ (concatMap (\p -> " " ++ pretty p) ps) ++ ")"

instance Pretty Expr where
    pretty (Var n) = n
    pretty (Lit l) = pretty l
    pretty (App e1@(Var _) e2@(Var _)) = pretty e1 ++ " " ++ pretty e2
    pretty (App e1@(Var _) e2@(Lit _)) = pretty e1 ++ " " ++ pretty e2
    pretty (App e1@(Var _) e2) = pretty e1 ++ " (" ++ pretty e2 ++ ")"
    pretty (App e1 e2@(Var _)) = "(" ++ pretty e1 ++ ") " ++ pretty e2
    pretty (App e1 e2@(Lit _)) = "(" ++ pretty e1 ++ ") " ++ pretty e2
    pretty (App e1 e2) = "(" ++ pretty e1 ++ ") (" ++ pretty e2 ++ ")"
    pretty (If cnd thn els) = "if " ++ pretty cnd ++ " then " ++ pretty thn ++ " else " ++ pretty els
    pretty (Case expr lst) = "case (" ++ pretty expr ++ ") of {" ++ concatMap (\(p, b) -> " |" ++ pretty p ++ " -> " ++ pretty b) lst ++ "}"
    pretty (Let name e1 e2) = "let " ++ name ++ " = " ++ pretty e1 ++ " in " ++ pretty e2
    pretty (UnOp op e@(Var _)) = pretty op ++ pretty e
    pretty (UnOp op e@(Lit _)) = pretty op ++ pretty e
    pretty (UnOp op e) = pretty op ++ "(" ++ pretty e ++ ")"
    pretty (BinOp op e1 e2) = pretty e1 ++ " " ++ pretty op ++ " " ++ pretty e2


instance Pretty Lit where
    pretty (ILit i) = show i
    pretty (BLit True) = "T"
    pretty (BLit False) = "F"

instance Pretty UnOperator where
    pretty Neg = "!"
    pretty Minus = "-"

instance Pretty ArithOperator where
    pretty Pow = "^"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Sum = "+"
    pretty Sub = ".-"

instance Pretty CmpOperator where
    pretty Eq = "=="
    pretty Neq = "/="
    pretty Le = "<="
    pretty Lt = "<"
    pretty Ge = ">="
    pretty Gt = ">"

instance Pretty LogicalOperator where
    pretty And = "&&"
    pretty Or  = "||"

instance Pretty BinOperator where
    pretty (AO o) = pretty o
    pretty (CO o) = pretty o
    pretty (LO o) = pretty o

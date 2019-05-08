{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TypeChecker where

import Ast
import Data.List (groupBy, intersect)

type ConstrName = Name
type DataCtx = [(ConstrName, Type)]

type NameCtx = [(Name, Type)]

instance Pretty NameCtx where
    pretty = unlines . fmap (\(n, t) -> ">> " ++ n ++ " :: " ++ pretty t)


typeCheckerCommon :: Ast -> Name -> Either String (Maybe Type)
typeCheckerCommon ast name = (lookup name) <$> typeCheckerCtx ast

typeCheckerCtx :: Ast -> Either String NameCtx
typeCheckerCtx ds = typeChecker' [] [] ds

typeChecker' :: DataCtx -> NameCtx -> [Decl] -> Either String NameCtx
typeChecker' _ c [] = Right c
typeChecker' dCtx fCtx (d:ds)
  | DataDecl dt@(Data name ctrs) <- d
  , Nothing <- lookup name dCtx = let ext = (ctrToType name <$> ctrs) in typeChecker' (ext ++ dCtx) (ext ++ fCtx) ds
  | DataDecl (Data name _) <- d
  , Just _ <- lookup name dCtx = Left $ "Data type '" ++ name ++ "' has already been declared!"

  | TypeDecl name tp <- d
  , Nothing <- lookup name fCtx = typeChecker' dCtx ((name, tp):fCtx) ds
  | TypeDecl name _  <- d
  , Just _ <- lookup name fCtx = Left $ "Type of function '" ++ name ++ "' has already been declared!"

  | BindDecl bind@(Bind name _ _) <- d
  , Just tp <- lookup name fCtx = do
      ctx <- checkBind dCtx fCtx tp bind
      typeChecker' dCtx ctx ds
  | otherwise = Left "Function declared without its type!"

ctrToType name (Constr cName names) = (cName, f name names)
  where
    f name [] = nameToType name
    f name (c:cs) = let t = f name cs in Arrow (nameToType c) t

nameToType "Int"  = TInt
nameToType "Bool" = TBool
nameToType n      = TData n

checkBind :: DataCtx -> NameCtx -> Type -> Bind -> Either String NameCtx
checkBind dCtx nCtx tp (Bind _ pats expr) = let
  (patTypes, resultType) = destructType tp
  in if length patTypes == length pats
     then do
         ctxs <- mapM (uncurry (checkPat dCtx nCtx)) (zip patTypes pats)
         if disjoint ctxs then do
             let ctx = (concat ctxs) ++ nCtx
             checkExpr dCtx ctx  expr
             Right nCtx
         else Left $ "Problems with patterns in function arguments."
     else Left "Function has different arity!"

checkPat :: DataCtx -> NameCtx -> Type -> Pat -> Either String NameCtx
checkPat _ ctx expectedType (PatVar x)
  | Just tp <- lookup x ctx = Left $ "Name '" ++ x ++ "' is already bound: '" ++ pretty tp ++"'!"
  | otherwise = Right [(x, expectedType)]
checkPat _ ctx TInt (PatLit (ILit _)) = Right []
checkPat _ ctx TBool (PatLit (BLit _)) = Right []
checkPat dCtx fCtx expectedType (PatCtr n ps)
 | Just t <- lookup n dCtx = do
     let (argTypes, resType) = destructType t
     if resType == expectedType
     then if length argTypes == length ps then do
         ctxs <- mapM (uncurry (checkPat dCtx fCtx)) (zip argTypes ps)
         if disjoint ctxs then
             Right (concat ctxs)
         else Left $ "Boo3: " ++ show ctxs
       else Left $ "Different number of arguments in the constructor '" ++ n ++ "' and in its type."
     else Left $ "Type '" ++ pretty expectedType ++ "' of the constructor '" ++ n ++ "' doesn't match its usage."
checkPat _ _ _ _ = Left "Pat doesn't match expected type"

disjoint :: Eq a => [[a]] -> Bool
disjoint = fst . foldr (\n (s, acc) ->
  (s && (null (intersect n acc)), n ++ acc)) (True, [])

destructType :: Type -> ([Type], Type)
destructType TInt = ([], TInt)
destructType TBool = ([], TBool)
destructType (TData a) = ([], TData a)
destructType (Arrow t1 t2) = let (ts, t) = destructType t2 in (t1:ts, t)


checkExpr :: DataCtx -> NameCtx -> Expr -> Either String Type
checkExpr _ fCtx (Var n)
  | Just x <- lookup n fCtx = Right x
  | otherwise = Left $ "Variable '" ++ n ++ "' not defined!"
checkExpr _ _ (Lit (BLit _)) = Right TBool
checkExpr _ _ (Lit (ILit _)) = Right TInt
checkExpr dCtx fCtx (App e1 e2) = do
  t1 <- checkExpr dCtx fCtx e1
  t2 <- checkExpr dCtx fCtx e2
  case t1 of
    (Arrow c d) -> if t2 == c then Right d else Left "XXX"
    _ -> Left "Wrong type for application!"

checkExpr dCtx fCtx (If cnd thn els) = do
  cndT <- checkExpr dCtx fCtx cnd
  thnT <- checkExpr dCtx fCtx thn
  elsT <- checkExpr dCtx fCtx els
  case cndT of
      TBool -> if thnT == elsT
               then Right thnT
               else Left "If's branches aren't of the same type."
      x     -> Left "Condition don't have boolean type"
checkExpr dCtx fCtx (Case expr ps) = do
  t <- checkExpr dCtx fCtx expr
  checkExprCasePat dCtx fCtx t ps
  pure t
  where
    checkExprCasePat _ _ _ [] = Left "Case without patterns!"
    checkExprCasePat dCtx fCtx t ((pat, expr):ps)= do
        ctxExt  <- checkPat dCtx fCtx t pat
        exprT <- checkExpr dCtx (ctxExt ++ fCtx) expr
        checkECP dCtx fCtx t exprT ps
      where
        checkECP _ _ _ et [] = Right et
        checkECP dCtx fCtx t et ((pt, ex):ps) = do
            ctx <- checkPat dCtx fCtx t pt
            eT <- checkExpr dCtx (ctx ++ fCtx) ex
            if (eT == et)
            then checkECP dCtx fCtx t et ps
            else Left "Pattern type or expression type doesn't match expected type."

checkExpr dCtx fCtx (Let name expr body) = do
  te <- checkExpr dCtx fCtx expr
  checkExpr dCtx ((name, te):fCtx) body

checkExpr dCtx fCtx (UnOp op e) = do
  t <- checkExpr dCtx fCtx e
  case op of
    Minus -> if t == TInt  then Right TInt else Left $ "Expression is not of integer type!"
    Neg   -> if t == TBool then Right TBool else Left $ "Expression is not of boolean type!"
checkExpr dCtx fCtx (BinOp op e1 e2) = do
  t1 <- checkExpr dCtx fCtx e1
  t2 <- checkExpr dCtx fCtx e2
  case op of
    AO _ -> if t1 == TInt && t2 == TInt then Right TInt else Left "Expressions for arith operations aren't of integer type!"
    LO _ -> if t1 == TBool && t2 == TBool then Right TBool else Left "Expressions for arith operations aren't of boolean type!"
    CO _ -> if (t1 == TInt && t2 == TInt) || (t1 == TBool && t2 == TBool) then Right TBool else Left "Expressions for arith operations aren't of integer of boolean type!"

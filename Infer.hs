module Infer where

import Data.List (group)
import Ast
import TypeChecker
import Parser

inferExpr :: Ast -> String -> Maybe Type
inferExpr ast expr
 | Right (_, ast') <- exprParser expr
 = infer0 ast ast'
inferExpr _ _ = Nothing

infer0 :: Ast -> Expr -> Maybe Type
infer0 ast expr = case typeCheckerCtx' ast of
                    Right c -> infer c expr
                    _ -> Nothing

infer :: (DataCtx, NameCtx) -> Expr -> Maybe Type
infer ctx (Var n)
  | Just t <- lookup n (snd ctx)
  = Just t
infer _ (Lit (ILit _))
  = Just TInt
infer _ (Lit (BLit _))
  = Just TBool
--
-- f : -> a -> a -> Int
-- f 3 True
--
-- NOTE: У нас нет частичного применения.
infer ctx (App e1 e2)
  | Just (Arrow f t) <- infer ctx e1
  , Just t2 <- infer ctx e2
  , f == t2
  = Just t

infer ctx (If cnd thn els)
  | Just TBool <- infer ctx cnd
  , Just t1    <- infer ctx thn
  , Just t2    <- infer ctx els
  , t1 == t2
  = Just t1
infer ctx (Let name e1 e2)
  | Just t1 <- infer ctx e1
  = infer (((name, t1):) <$> ctx) e2
infer ctx (UnOp Minus e)
  | Just  TInt <- infer ctx e
  = Just TInt
infer ctx (UnOp Neg e)
  | Just TBool <- infer ctx e
  = Just TBool
infer ctx (BinOp (AO _) e1 e2)
  | Just TInt <- infer ctx e1
  , Just TInt <- infer ctx e2
  = Just TInt
infer ctx (BinOp (CO _) e1 e2)
  | Just TInt <- infer ctx e1
  , Just TInt <- infer ctx e2
  = Just TInt
infer ctx (BinOp (LO _) e1 e2)
  | Just TBool <- infer ctx e1
  , Just TBool <- infer ctx e2
  = Just TBool
infer ctx@(dctx, nctx) (Case e entries)
  | Just t1 <- infer ctx e
  , all (isPatOfType (fst ctx) t1 . fst) entries
  = let l = group $ inferBody t1 <$> entries
    in if length l == 1
       then head (head l)
       else Nothing
  where
    inferBody t (pat, body) = infer (dctx,  extend t nctx pat) body
    extend :: Type -> NameCtx -> Pat -> NameCtx
    extend t ctx (PatVar x) = (x, t):ctx
    extend _ ctx (PatLit _) = ctx
    extend t ctx (PatCtr n pats) = foldr (\p c -> extend t c p) ctx pats
infer _ _ = Nothing

isPatOfType :: DataCtx -> Type -> Pat -> Bool
isPatOfType _ typ (PatVar x)
  | Arrow t1 t2 <- typ
  = False
  | otherwise
  = True
isPatOfType _ TBool (PatLit (BLit _)) = True
isPatOfType _ TInt (PatLit (ILit _)) = True
isPatOfType ctx typ@(TData _) (PatCtr name pats)
  | Just typ' <- lookup name ctx
  , (ts, t)  <- destructType typ'
  , typ == t
  , length ts == length pats
  = all (uncurry (isPatOfType ctx)) $ zip ts pats
isPatOfType _ _ _ = False

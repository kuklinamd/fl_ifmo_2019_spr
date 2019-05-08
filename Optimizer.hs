module Optimizer (optimize) where

import Expression
import Debug.Trace

{-
Optimizations:
* partial eval +
* Ops with vars: +
  > t+0 = t
  > t*1 = t
  > t^1 = t
  > t^0 = 1
  > t*0 = 0
  > 0 div t = 0
  > t div 0 = error

  > 1 && x = x
  > 0 && x = 0

  > 0 || x = x
  > 1 || x = 1
-}

optimize :: EAst Integer -> EAst Integer
optimize t = let t' = cut $ partialEval t
  in if t == t'
      then t
      else optimize t'

cut :: EAst Integer -> EAst Integer
-- Id
cut (BinOp Sum (Primary 0) t) = t
cut (BinOp Sum t (Primary 0)) = t
cut (BinOp Mul (Primary 1) t) = t
cut (BinOp Mul t (Primary 1)) = t
cut (BinOp Pow t (Primary 1)) = t
-- Always zero or one
cut (BinOp Mul t (Primary 0)) = Primary 0
cut (BinOp Mul (Primary 0) t) = Primary 0
cut (BinOp Div (Primary 0) t) = Primary 0
cut (BinOp Div t (Primary 0)) = error "Division by zero!"
cut (BinOp Div t (Primary 1)) = t
cut (BinOp Pow t (Primary 0)) = Primary 1
cut (BinOp Pow (Primary 0) t) = Primary 0
cut (BinOp Pow (Primary 1) t) = Primary 1
cut (BinOp Conj (Primary x) t) | toB x = t
                               | otherwise = Primary 0
cut (BinOp Conj t (Primary x)) | toB x = t
                               | otherwise = Primary 0
cut (BinOp Disj (Primary x) t) | toB x = Primary 1
                               | otherwise = t
cut (BinOp Disj t (Primary x)) | toB x = Primary 1
                               | otherwise = t
cut t@(BinOp op t1 t2) = let t' = BinOp op (cut t1) (cut t2)
 in if t' == t then t else cut t'
cut t@(UnOp op t1) = let t' = UnOp op (cut t1)
 in if t' == t then t else cut t'
cut x = x

partialEval :: EAst Integer -> EAst Integer
partialEval (UnOp op t) =
  case partialEval t of
    Primary x | Just f <- lookup op uops -> Primary $ f x
    x -> UnOp op x
partialEval (BinOp op t1 t2) = let
  optt1 = partialEval t1
  optt2 = partialEval t2
  in case (optt1, optt2) of
    (Primary x, Primary y) | Just f <- lookup op bops -> Primary $ f x y
    _ -> BinOp op optt1 optt2
partialEval x = x

uops = [(Minus, \x -> -x)
       ,(Neg, (toIfE . not . toB))]

bops = [ (Pow, (^))
       , (Mul, (*))
       , (Div, (div))
       , (Sum, (+))
       , (Sub, (-))
       , (Eq,   (\i1 i2 -> toIfE (i1 == i2)))
       , (Neq,  (\i1 i2 -> toIfE (i1 /= i2)))
       , (Le,   (\i1 i2 -> toIfE (i1 <= i2)))
       , (Lt,   (\i1 i2 -> toIfE (i1 < i2)))
       , (Ge,   (\i1 i2 -> toIfE (i1 >= i2)))
       , (Gt,   (\i1 i2 -> toIfE (i1 > i2)))
       , (Conj, (\i1 i2 -> toIfE (toB i1 && toB i2)))
       , (Disj, (\i1 i2 -> toIfE (toB i1 || toB i2)))]

toIfE = toInteger . fromEnum
toB = (0 /=)

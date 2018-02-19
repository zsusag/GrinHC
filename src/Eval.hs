{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Eval where

import Lang
import Error

evaluate :: Exp Pos -> String
evaluate = show . simplify

simplify :: Exp Pos -> Value
simplify (PosExp _ (EInt n)) = VInt n
simplify (PosExp _ (EBool b)) = VBool b
simplify (PosExp _ (EFloat f)) = VFloat f
simplify (PosExp _ ENaN) = VNaN
simplify (PosExp p (EOp op e1 e2)) = 
  case (simplify e1, simplify e2) of
    (VInt n1, VInt n2) -> intOp p op n1 n2
    (VInt n, VFloat f) -> floatOp p op (fromIntegral n) f
    (VFloat f, VInt n) -> floatOp p op f (fromIntegral n)
    (VFloat f1, VFloat f2) -> floatOp p op f1 f2
    (VNaN, _) -> VNaN
    (_, VNaN) -> VNaN
    _ -> posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
simplify (PosExp p (EIf e1 e2 e3)) = if b then simplify e2 else simplify e3
  where b = case simplify e1 of
              (VBool b') -> b'
              _ -> posError p "Evaluation Error" ": expected a boolean value in guard of conditional"

intOp :: Pos -> Op -> Int -> Int -> Value
intOp _ Plus n1 n2  = VInt $ n1 + n2
intOp _ Minus n1 n2 = VInt $ n1 - n2
intOp _ Mult n1 n2  = VInt $ n1 * n2
intOp _ Lte n1 n2   = VBool $ n1 <= n2
intOp _ Geq n1 n2   = VBool $ n1 >= n2
intOp _ Eq n1 n2    = VBool $ n1 == n2
intOp p Div n1 n2
  | n1 == 0 && n2 == 0 = VNaN
  | n2 == 0 = posError p "Evaluation Erro" ": divide by zero"
  | otherwise = VInt $ n1 `div` n2
{-# INLINE intOp #-}

floatOp :: Pos -> Op -> Float -> Float -> Value
floatOp _ Plus f1 f2  = VFloat $ f1 + f2 
floatOp _ Minus f1 f2 = VFloat $ f1 - f2
floatOp _ Mult f1 f2  = VFloat $ f1 * f2
floatOp _ Lte f1 f2   = VBool $ f1 <= f2
floatOp _ Geq f1 f2   = VBool $ f1 >= f2
floatOp _ Eq f1 f2    = VBool $ f1 == f2
floatOp p Div f1 f2
  | f1 == 0 && f2 == 0 = VNaN
  | f2 == 0 = posError p "Evaluation Erro" ": divide by zero"
  | otherwise = VFloat $ f1 / f2
{-# INLINE floatOp #-}

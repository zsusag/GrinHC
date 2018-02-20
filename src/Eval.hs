module Eval where

import Lang
import Error

evaluate :: Exp Pos -> String
evaluate = show . simplify

evaluateToExp :: Exp Pos -> Exp_ Pos
evaluateToExp (PosExp p e) = case simplify (PosExp p e) of
  (VInt n)  -> EInt n
  (VBool b) -> EBool b
  (VFloat f) -> EFloat f
  (VFun s e') -> EFun (PosExp p (ELid s)) e'
  (VRec f s e') -> ERec (PosExp p (ELid f)) (PosExp p (ELid s)) e'
  VNaN -> ENaN

simplify :: Exp Pos -> Value
simplify (PosExp _ (EInt n)) = VInt n
simplify (PosExp _ (EBool b)) = VBool b
simplify (PosExp _ (EFloat f)) = VFloat f
simplify (PosExp p (ELid _)) = posError p "Evaluation Error" "Dangling identifier"
simplify (PosExp _ ENaN) = VNaN
simplify (PosExp p (EFun e1 e2)) = case e1 of
  (PosExp _ (ELid s)) -> VFun s e2
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
simplify (PosExp p (ERec e1 e2 e3)) = case e1 of
  (PosExp _ (ELid f)) -> case e2 of
    (PosExp _ (ELid s)) -> VRec f s e3
    _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
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
simplify (PosExp p (ELet e1 e2 e3)) = case e1 of
  (PosExp _ (ELid s)) -> let v = evaluateToExp e2
    in simplify $ subst v (ELid s) e3
  _ -> posError p "Evaluation Error" ": variable for let-binding is not an identifier"
simplify (PosExp p (EFunApp e1 e2)) = let e1' = evaluateToExp e1
  in case e1' of
  (EFun (PosExp _ (ELid l)) e) -> let v = evaluateToExp e2
    in simplify $ subst v (ELid l) e
  (ERec (PosExp _ (ELid f)) (PosExp _ (ELid l)) e) ->
    let v = evaluateToExp e2
    in simplify $ subst e1' (ELid f) (subst v (ELid l) e)
  _ -> posError p "Evaluation Error" ": expression does not evaluate to function"

subst :: Exp_ Pos -> Exp_ Pos -> Exp Pos -> Exp Pos
subst val var (PosExp p e) = if e == var
  then PosExp p val
  else case e of
  (EFun (PosExp p' e1) e2) -> if e1 == var
      then PosExp p e
      else PosExp p $ EFun (subst val var (PosExp p' e1)) (subst val var e2)
  (EOp op e1 e2) ->
    PosExp p $ EOp op (subst val var e1) (subst val var e2)
  (EIf e1 e2 e3) ->
    PosExp p $ EIf (subst val var e1) (subst val var e2) (subst val var e3)
  (ELet (PosExp p' e1) e2 e3) -> if e1 == var
      then PosExp p (ELet (PosExp p' e1) (subst val var e2) e3)
      else PosExp p $ ELet (subst val var (PosExp p' e1)) (subst val var e2) (subst val var e3)
  (EFunApp e1 e2) ->
    PosExp p $ EFunApp (subst val var e1) (subst val var e2)
  _ -> PosExp p e

intOp :: Pos -> Op -> Int -> Int -> Value
intOp _ Plus n1 n2  = VInt $ n1 + n2
intOp _ Minus n1 n2 = VInt $ n1 - n2
intOp _ Mult n1 n2  = VInt $ n1 * n2
intOp _ Lte n1 n2   = VBool $ n1 <= n2
intOp _ Geq n1 n2   = VBool $ n1 >= n2
intOp _ Eq n1 n2    = VBool $ n1 == n2
intOp _ Lt n1 n2    = VBool $ n1 < n2
intOp _ Gt n1 n2    = VBool $ n1 > n2
intOp p Div n1 n2
  | n1 == 0 && n2 == 0 = VNaN
  | n2 == 0 = posError p "Evaluation Error" ": divide by zero"
  | otherwise = VInt $ n1 `div` n2
{-# INLINE intOp #-}

floatOp :: Pos -> Op -> Float -> Float -> Value
floatOp _ Plus f1 f2  = VFloat $ f1 + f2 
floatOp _ Minus f1 f2 = VFloat $ f1 - f2
floatOp _ Mult f1 f2  = VFloat $ f1 * f2
floatOp _ Lte f1 f2   = VBool $ f1 <= f2
floatOp _ Geq f1 f2   = VBool $ f1 >= f2
floatOp _ Eq f1 f2    = VBool $ f1 == f2
floatOp _ Lt f1 f2    = VBool $ f1 < f2
floatOp _ Gt f1 f2    = VBool $ f1 > f2
floatOp p Div f1 f2
  | f1 == 0 && f2 == 0 = VNaN
  | f2 == 0 = posError p "Evaluation Error" ": divide by zero"
  | otherwise = VFloat $ f1 / f2
{-# INLINE floatOp #-}

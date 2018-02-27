module Eval where

import Lang
import Error

evaluate :: Exp Pos -> Bool -> IO ()
evaluate e b
 | isValue e = print $ expToValue e
 | b = do
     print e
     evaluate (step e) b
 | otherwise = evaluate (step e) b

step :: Exp Pos -> Exp Pos
step (PosExp p _ (EVar _)) = posError p "Evaluation Error" ": dangling identifier"
step e@(PosExp p _ (EFun e1 _)) = case e1 of
  (PosExp _ _ (EVar _)) -> e
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
step e@(PosExp p _ (ERec e1 e2 _)) = case e1 of
  (PosExp _ _ (EVar _)) -> case e2 of
    (PosExp _ _ (EVar _)) -> e
    _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
step (PosExp p t (EOp op e1 e2))
  | isValue e1 && isValue e2 = case (e1,e2) of
      (PosExp _ _ (EInt n1), PosExp _ _ (EInt n2)) -> intOp p op n1 n2
      (PosExp _ _ (EInt n), PosExp _ _ (EFloat f)) -> floatOp p op (fromIntegral n) f
      (PosExp _ _ (EFloat f), PosExp _ _ (EInt n)) -> floatOp p op f (fromIntegral n)
      (PosExp _ _ (EFloat f1), PosExp _ _ (EFloat f2)) -> floatOp p op f1 f2
      (PosExp _ _ ENaN, _) -> e1
      (_, PosExp _ _ ENaN) -> e2
      _ -> posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
  | isValue e1 = PosExp p t (EOp op e1 (step e2))
  | otherwise = PosExp p t (EOp op (step e1) e2)
step (PosExp p t (EIf e1 e2 e3))
  | isValue e1 = case expToValue e1 of
      (VBool True) -> if isValue e2
                      then e2
                      else PosExp p t (EIf e1 (step e2) e3)
      (VBool False) -> if isValue e3
                       then e3
                       else PosExp p t (EIf e1 e2 (step e3))
      _ -> posError p "Evaluation Error" ": expected a boolean value in guard of conditional"
  | otherwise = PosExp p t (EIf (step e1) e2 e3)
step (PosExp p _ (ELet e1 e2 e3)) = case e1 of
  (PosExp _ t (EVar s))
    | isValue e2 && isValue e3 -> e3
    | isValue e2 -> subst (extractExp e2) (EVar s) e3
    | otherwise -> PosExp p t (ELet e1 (step e2) e3)
  _ -> posError p "Evaluation Error" ": variable for let-binding is not an identifier"
step (PosExp p t (EFunApp e1 e2))
  | isValue e1 && isValue e2 = let e1' = extractExp e1 in
      case e1' of
        (EFun (PosExp _ _ (EVar l)) e) -> subst (extractExp e2) (EVar l) e
        (ERec (PosExp _ _ (EVar f)) (PosExp _ _ (EVar l)) e) ->
          subst e1' (EVar f) (subst (extractExp e2) (EVar l) e)
        _ -> posError p "Evaluation Error" ": expression does not evaluate to a function"
  | isValue e1 = PosExp p t (EFunApp e1 (step e2))
  | otherwise = PosExp p t (EFunApp (step e1) e2)
step (PosExp p t e@(EPair e1 e2))
  | isValue e1 && isValue e2 = PosExp p t e
  | isValue e1 = PosExp p t $ EPair e1 $ step e2
  | otherwise = PosExp p t $ EPair (step e1) e2
step (PosExp p t (EFst e)) =  case e of
      (PosExp p' t' (EPair e1 e2)) -> if isValue e1
        then e1
        else PosExp p t $ EFst $ PosExp p' t' $ EPair (step e1) e2
      _ -> posError p "Evaluation Error" ": cannot call 'fst' on a non-pair"
step (PosExp p t (ESnd e)) =  case e of
      (PosExp p' t' (EPair e1 e2)) -> if isValue e2
        then e2
        else PosExp p t $ EFst $ PosExp p' t' $ EPair e1 $ step e2
      _ -> posError p "Evaluation Error" ": cannot call 'snd' on a non-pair"
step e@(PosExp _ _ ENil) = e
step e@(PosExp p t (ECons e1 e2))
  | isValue e1 && isValue e2 = e
  | isValue e1 = PosExp p t $ ECons e1 $ step e2
  | otherwise = PosExp p t $ ECons (step e1) e2
step (PosExp p t (EHead e)) = if isValue e
  then case e of
  (PosExp _ _ (ECons e1 _)) -> e1
  (PosExp p' _ ENil) -> posError p' "Evaluation Error" ": cannot call head on an empty list"
  _ -> posError p "Evaluation Error" ": cannot call head on a non-list"
  else PosExp p t $ EHead $ step e
step (PosExp p t (ETail e)) = if isValue e
  then case e of
  (PosExp _ _ (ECons _ e2)) -> e2
  (PosExp p' _ ENil) -> posError p' "Evaluation Error" ": cannot call head on an empty list"
  _ -> posError p "Evaluation Error" ": cannot call head on a non-list"
  else PosExp p t $ EHead $ step e
step (PosExp p t (EEmpty e)) = if isValue e
  then case e of
  (PosExp _ _ ENil) -> PosExp p TBool $ EBool True
  (PosExp _ _ (ECons _ _)) -> PosExp p TBool $ EBool False
  _ -> posError p "Evaluation Error" ": cannot call empty on a non-list"
  else PosExp p t $ EEmpty $ step e
step e = e

subst :: Exp_ Pos -> Exp_ Pos -> Exp Pos -> Exp Pos
subst val var (PosExp p t e) = if e == var
  then PosExp p t val
  else case e of
  (EFun (PosExp p' t' e1) e2) -> if e1 == var
      then PosExp p t' e
      else PosExp p t' $ EFun (subst val var (PosExp p' t' e1)) (subst val var e2)
  (ERec (PosExp p1 t' e1) (PosExp p2 _ e2) e3)
    | e1 == var -> PosExp p t' e
    | e2 == var -> PosExp p t' e
    | otherwise -> PosExp p t' $ ERec (subst val var (PosExp p1 t e1)) (subst val var (PosExp p2 t e2)) (subst val var e3) 
  (EOp op e1 e2) ->
    PosExp p t $ EOp op (subst val var e1) (subst val var e2)
  (EIf e1 e2 e3) ->
    PosExp p t $ EIf (subst val var e1) (subst val var e2) (subst val var e3)
  (ELet (PosExp p' t' e1) e2 e3) -> if e1 == var
      then PosExp p t' (ELet (PosExp p' t' e1) (subst val var e2) e3)
      else PosExp p t' $ ELet (subst val var (PosExp p' t' e1)) (subst val var e2) (subst val var e3)
  (EFunApp e1 e2) ->
    PosExp p t $ EFunApp (subst val var e1) (subst val var e2)
  (EPair e1 e2) -> PosExp p t $ EPair (subst val var e1) (subst val var e2)
  (EFst e') -> PosExp p t $ EFst $ subst val var e'
  (ESnd e') -> PosExp p t $ ESnd $ subst val var e'
  (ECons e1 e2) -> PosExp p t $ ECons (subst val var e1) (subst val var e2)
  (EHead e') -> PosExp p t $ EHead $ subst val var e'
  (ETail e') -> PosExp p t $ ETail $ subst val var e'
  (EEmpty e') -> PosExp p t $ EEmpty $ subst val var e'
  _ -> PosExp p t e

extractExp :: Exp Pos -> Exp_ Pos
extractExp (PosExp _ _ e@(EInt _)) = e
extractExp (PosExp _ _ e@(EFloat _)) = e
extractExp (PosExp _ _ e@(EBool _)) = e
extractExp (PosExp _ _ e@(EFun _ _)) = e
extractExp (PosExp _ _ e@ERec{}) = e
extractExp (PosExp _ _ ENaN) = ENaN
extractExp (PosExp _ _ EUnit) = EUnit
extractExp (PosExp _ _ e@(EPair _ _)) = e
extractExp (PosExp _ _ ENil) = ENil
extractExp (PosExp _ _ e@(ECons _ _)) = e
extractExp _ = error "This should never happen..."

expToValue :: Exp Pos -> Value
expToValue (PosExp _ _ (EInt n)) = VInt n
expToValue (PosExp _ _ (EFloat f)) = VFloat f
expToValue (PosExp _ _ (EBool b)) = VBool b
expToValue (PosExp _ _ (EFun (PosExp _ _ (EVar s)) e)) = VFun s e
expToValue (PosExp _ _ (ERec (PosExp _ _ (EVar f)) (PosExp _ _ (EVar s)) e)) = VRec f s e
expToValue (PosExp _ _ ENaN) = VNaN
expToValue (PosExp _ _ EUnit) = VUnit
expToValue (PosExp _ _ (EPair e1 e2)) = VPair (expToValue e1) (expToValue e2)
expToValue e@(PosExp _ _ ENil) = VList e
expToValue e@(PosExp _ _ (ECons _ _)) = VList e
expToValue (PosExp p _ _ ) = posError p "Evaluation Error" ": expression is not a value"

isValue :: Exp Pos -> Bool
isValue (PosExp _ _ (EInt _)) = True
isValue (PosExp _ _ (EBool _)) = True
isValue (PosExp _ _ (EFloat _)) = True
isValue (PosExp _ _ (EFun _ _)) = True
isValue (PosExp _ _ ERec{}) = True
isValue (PosExp _ _ ENaN) = True
isValue (PosExp _ _ EUnit) = True
isValue (PosExp _ _ (EPair e1 e2)) = isValue e1 && isValue e2
isValue (PosExp _ _ ENil) = True
isValue (PosExp _ _ (ECons e1 e2)) = isValue e1 && isValue e2
isValue _ = False

intOp :: Pos -> Op -> Int -> Int -> Exp Pos
intOp p Plus n1 n2  = PosExp p TUnit (EInt $ n1 + n2)
intOp p Minus n1 n2 = PosExp p TUnit (EInt $ n1 - n2)
intOp p Mult n1 n2  = PosExp p TUnit (EInt $ n1 * n2)
intOp p Lte n1 n2   = PosExp p TUnit (EBool $ n1 <= n2)
intOp p Geq n1 n2   = PosExp p TUnit (EBool $ n1 >= n2)
intOp p Eq n1 n2    = PosExp p TUnit (EBool $ n1 == n2)
intOp p Lt n1 n2    = PosExp p TUnit (EBool $ n1 < n2)
intOp p Gt n1 n2    = PosExp p TUnit (EBool $ n1 > n2)
intOp p Div n1 n2
  | n1 == 0 && n2 == 0 = PosExp p TUnit  ENaN
  | n2 == 0 = posError p "Evaluation Error" ": divide by zero"
  | otherwise = PosExp p TUnit  (EInt $ n1 `div` n2)
intOp p Mod n1 n2
  | n1 == 0 && n2 == 0 = PosExp p TUnit  ENaN
  | n2 == 0 = posError p "Evaluation Error" ": divide by zero"
  | otherwise = PosExp p TUnit  (EInt $ n1 `mod` n2)
{-# INLINE intOp #-}

floatOp :: Pos -> Op -> Float -> Float -> Exp Pos
floatOp p Plus f1 f2  = PosExp p TUnit  (EFloat $ f1 + f2)
floatOp p Minus f1 f2 = PosExp p TUnit  (EFloat $ f1 - f2)
floatOp p Mult f1 f2  = PosExp p TUnit  (EFloat $ f1 * f2)
floatOp p Lte f1 f2   = PosExp p TUnit  (EBool $ f1 <= f2)
floatOp p Geq f1 f2   = PosExp p TUnit  (EBool $ f1 >= f2)
floatOp p Eq f1 f2    = PosExp p TUnit  (EBool $ f1 == f2)
floatOp p Lt f1 f2    = PosExp p TUnit  (EBool $ f1 < f2)
floatOp p Gt f1 f2    = PosExp p TUnit  (EBool $ f1 > f2)
floatOp p Div f1 f2
  | f1 == 0 && f2 == 0 = PosExp p TUnit  ENaN
  | f2 == 0 = posError p "Evaluation Error" ": divide by zero"
  | otherwise = PosExp p TUnit  (EFloat $ f1 / f2)
floatOp p Mod _ _ = posError p "Evaluation Error" ": cannot take the modulus of a float"

{-# INLINE floatOp #-}

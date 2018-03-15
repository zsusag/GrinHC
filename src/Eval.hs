module Eval where

import Lang
import Error
import qualified Data.Map.Strict as Map

evaluate :: Exp Pos -> Bool -> Env -> IO ()
evaluate e b env
 | isValue e = print $ expToValue e
 | b = do
     print e
     let (e',env') = step (e,env)
     evaluate e' b env'
 | otherwise = let (e',env') = step (e,env)
               in evaluate e' b env'

step :: (Exp Pos, Env) -> (Exp Pos, Env)
step (PosExp p _ (EVar s),_) = posError p "Evaluation Error" (": dangling identifier" ++ s)
step (e@(PosExp p _ (EFun e1 _)),env) = case e1 of
  (PosExp _ _ (EVar _)) -> (e,env)
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
step (e@(PosExp p _ (ERec e1 e2 _)),env) = case e1 of
  (PosExp _ _ (EVar _)) -> case e2 of
    (PosExp _ _ (EVar _)) -> (e,env)
    _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
  _ -> posError p "Evaluation Error" ": variable for function is not an identifier"
step (PosExp p t (EOp op e1 e2),env)
  | isValue e1 && isValue e2 = case (e1,e2) of
      (PosExp _ _ (EInt n1), PosExp _ _ (EInt n2)) -> (intOp p op n1 n2,env)
      (PosExp _ _ (EInt n), PosExp _ _ (EFloat f)) -> (floatOp p op (fromIntegral n) f,env)
      (PosExp _ _ (EFloat f), PosExp _ _ (EInt n)) -> (floatOp p op f (fromIntegral n),env)
      (PosExp _ _ (EFloat f1), PosExp _ _ (EFloat f2)) -> (floatOp p op f1 f2,env)
      (PosExp _ _ ENaN, _) -> (e1,env)
      (_, PosExp _ _ ENaN) -> (e2,env)
      _ -> posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
  | isValue e1 = let (e,env') = step (e2,env)
                 in (PosExp p t (EOp op e1 e),env')
  | otherwise = let (e,env') = step (e1,env)
                in (PosExp p t (EOp op e e2), env')
step (PosExp p t (EIf e1 e2 e3),env)
  | isValue e1 = case expToValue e1 of
      (VBool True) -> if isValue e2
                      then (e2,env)
                      else let (e,env') = step (e2,env)
                           in (PosExp p t (EIf e1 e e3),env')
      (VBool False) -> if isValue e3
                       then (e3,env)
                       else let (e,env') = step (e3,env)
                            in (PosExp p t (EIf e1 e2 e),env')
      _ -> posError p "Evaluation Error" ": expected a boolean value in guard of conditional"
  | otherwise = let (e,env') = step (e1,env)
                in (PosExp p t (EIf e e2 e3), env')
step (PosExp p _ (ELet e1 e2 e3),env) = case e1 of
  (PosExp _ t (EVar s))
    | isValue e2 && isValue e3 -> (e3,env)
    | isValue e2 -> (subst (extractExp e2) (EVar s) e3,env)
    | otherwise -> let (e,env') = step (e2,env)
                   in (PosExp p t (ELet e1 e e3),env')
  _ -> posError p "Evaluation Error" ": variable for let-binding is not an identifier"
step (PosExp p t (EFunApp e1 e2),env)
  | isValue e1 && isValue e2 = let e1' = extractExp e1 in
      case e1' of
        (EFun (PosExp _ _ (EVar l)) e) -> (subst (extractExp e2) (EVar l) e,env)
        (ERec (PosExp _ _ (EVar f)) (PosExp _ _ (EVar l)) e) ->
          (subst e1' (EVar f) (subst (extractExp e2) (EVar l) e),env)
        _ -> posError p "Evaluation Error" ": expression does not evaluate to a function"
  | isValue e1 = let (e,env') = step (e2,env)
                 in (PosExp p t (EFunApp e1 e),env')
  | otherwise = let (e,env') = step (e1, env)
                in (PosExp p t (EFunApp e e2),env')
step (PosExp p t e@(EPair e1 e2),env)
  | isValue e1 && isValue e2 = (PosExp p t e,env)
  | isValue e1 = let (e',env') = step (e2,env)
                 in (PosExp p t $ EPair e1 e',env')
  | otherwise = let (e',env') = step (e1,env)
                in (PosExp p t $ EPair e' e2,env')
step (PosExp p t (EFst e),env) =  case e of
      (PosExp p' t' (EPair e1 e2)) -> if isValue e1
        then (e1,env)
        else let (e1',env') = step (e1,env)
             in (PosExp p t $ EFst $ PosExp p' t' $ EPair e1' e2,env')
      _ -> posError p "Evaluation Error" ": cannot call 'fst' on a non-pair"
step (PosExp p t (ESnd e),env) =  case e of
      (PosExp p' t' (EPair e1 e2)) -> if isValue e2
        then (e2,env)
        else let (e2',env') = step (e2,env)
        in (PosExp p t $ EFst $ PosExp p' t' $ EPair e1 e2',env')
      _ -> posError p "Evaluation Error" ": cannot call 'snd' on a non-pair"
step (e@(PosExp _ _ ENil),env) = (e,env)
step (e@(PosExp p t (ECons e1 e2)),env)
  | isValue e1 && isValue e2 = (e,env)
  | isValue e1 = let (e2',env') = step (e2,env)
                 in (PosExp p t $ ECons e1 e2',env')
  | otherwise = let (e1',env') = step (e1,env)
                in (PosExp p t $ ECons e1' e2,env')
step (PosExp p t (EHead e),env) = if isValue e
  then case e of
  (PosExp _ _ (ECons e1 _)) -> (e1,env)
  (PosExp p' _ ENil) -> posError p' "Evaluation Error" ": cannot call head on an empty list"
  _ -> posError p "Evaluation Error" ": cannot call head on a non-list"
  else let (e',env') = step (e,env)
       in (PosExp p t $ EHead e',env')
step (PosExp p t (ETail e),env) = if isValue e
  then case e of
  (PosExp _ _ (ECons _ e2)) -> (e2,env)
  (PosExp p' _ ENil) -> posError p' "Evaluation Error" ": cannot call head on an empty list"
  _ -> posError p "Evaluation Error" ": cannot call head on a non-list"
  else let (e',env') = step (e,env)
       in (PosExp p t $ EHead e',env')
step (PosExp p t (EEmpty e),env) = if isValue e
  then case e of
  (PosExp _ _ ENil) -> (PosExp p TBool $ EBool True,env)
  (PosExp _ _ (ECons _ _)) -> (PosExp p TBool $ EBool False,env)
  _ -> posError p "Evaluation Error" ": cannot call empty on a non-list"
  else let (e',env') = step (e,env)
       in (PosExp p t $ EEmpty e',env')
step (PosExp p t (ERef e),env)
  | isValue e = let m = Map.insert n e (snd env)
                in (PosExp p t $ EPtr n,(n+1,m))
  | otherwise = let (e',env') = step (e,env)
                in (PosExp p t $ ERef e', env')
  where n = fst env
step (PosExp p t (ESet e1 e2),env)
  | isValue e1 && isValue e2 = case e1 of
      (PosExp _ _ (EPtr n)) -> let m = Map.update (\_ -> Just e2) n (snd env)
        in (PosExp p t EUnit,(fst env,m))
      _ -> posError p "Evaluation Error" ": cannot set value of a non-ref variable"
  | isValue e1 = let (e2',env') = step (e2,env)
                 in (PosExp p t (ESet e1 e2'),env')
  | otherwise = let (e1',env') = step (e1,env)
                in (PosExp p t (ESet e1' e2),env')
step (PosExp p t (EBang e),env) = if isValue e
  then case e of
         (PosExp _ _ (EPtr n)) -> case Map.lookup n $ snd env of
           (Just e') -> (e',env)
           Nothing -> posError p "Evaluation Error" (": no reference to " ++ show e ++ " in environment")
         _ -> posError p "Evaluation Error" ": cannot dereference non-ref variable"
  else let (e',env') = step (e,env)
       in (PosExp p t $ EBang e', env')
step (PosExp p t (ESeq e1 e2),env)
  | isValue e1 = step (e2,env)
  | otherwise = let (e1',env') = step (e1,env)
                in (PosExp p t (ESeq e1' e2),env')
step (PosExp p t e@(EWhile e1 e2), env) = (PosExp p t (EIf e1 (PosExp p t (ESeq e2 (PosExp p t e))) (PosExp p t EUnit)),env)
step (PosExp p t (ECtor s es),env) = (PosExp p t (ECtor s $ map (\e -> let (e',_) = step (e,env) in e') es), env)
step (PosExp p t (EMatch e bs), env)
  | isValue e = case e of
      (PosExp _ _ (EPair e1 e2)) -> let checkPairPattern :: Branch Pos -> Bool
                                        checkPairPattern (p',_) = case p' of
                                          (PPair _) -> True
                                          PWildCard -> True
                                          _ -> False
        in case head $ filter checkPairPattern bs of
        (PPair (PVar x1, PVar x2),e') -> let e1' = extractExp e1
                                             e2' = extractExp e2
                                             var1 = EVar x1
                                             var2 = EVar x2
          in step (PosExp p t $ extractExp $ subst e1' var1 $ subst e2' var2 e', env)
        _ -> posError p "Evaluation Error" ": cannot match a pair using a non-pair pattern"
      (PosExp _ _ (ECtor x es)) -> let checkCtorPattern :: Branch Pos -> Bool
                                       checkCtorPattern (p',_) = case p' of
                                         (PCtor x' _) -> x == x'
                                         PWildCard -> True
                                         _ -> False
        in case head $ filter checkCtorPattern bs of
        (PCtor _ ps, e') ->
          let zipFn :: Pattern -> Exp Pos -> (Exp_ Pos, Exp_ Pos)
              zipFn (PVar s) e'' = (EVar s, extractExp e'')
              zipFn _ _ = posError p "Evaluation Error" ": pattern has non variable identifiers in constructor"
              substCtorVars :: [(Exp_ Pos, Exp_ Pos)] -> Exp Pos -> Exp Pos
              substCtorVars ((var,val):es') e'' = subst val var (substCtorVars es' e'')
              substCtorVars [] e''              = e''
          in let (e'',env') = step (substCtorVars (zipWith zipFn ps es) e',env)
             in (e'',env')
        (PWildCard, e') -> (PosExp p t $ extractExp e', env)
        _ -> posError p "Evaluation Error" ": cannot match a constructor using a non-constructor pattern"
      _ -> posError p "Evaluation Error" ": cannot pattern match on an expression that isn't a constructor, pair, or list"
  | otherwise = let (e',env') = step (e,env)
                in (PosExp p t (EMatch e' bs), env')
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
  (ERef e') -> PosExp p t $ ERef $ subst val var e'
  (ESet e1 e2) -> PosExp p t $ ESet (subst val var e1) (subst val var e2)
  (EBang e') -> PosExp p t $ EBang $ subst val var e'
  (ESeq e1 e2) -> PosExp p t $ ESeq (subst val var e1) (subst val var e2)
  (EWhile e1 e2) -> PosExp p t $ EWhile (subst val var e1) (subst val var e2)
  (ECtor x es) -> PosExp p t $ ECtor x $ map (subst val var) es
  (EMatch e' bs) ->
    let mapFn :: Branch Pos -> Branch Pos
        mapFn (PWildCard,e'') = (PWildCard, subst val var e'')
        mapFn (PVar s,e'') = if s == extractVar var
                             then (PVar s, e'')
                             else (PVar s, subst val var e'')
        mapFn (p'@(PPair (p1,p2)),e'') =
          if elem (extractVar var) (extractPatVars p1) ||
             elem (extractVar var) (extractPatVars p2)
          then (p', e'')
          else (p', subst val var e'')
        mapFn (p'@(PList (p1,p2)),e'') =
          if elem (extractVar var) (extractPatVars p1) ||
             elem (extractVar var) (extractPatVars p2)
          then (p', e'')
          else (p', subst val var e'')
        mapFn (p'@(PCtor _ _), e'') = if extractVar var `elem` extractPatVars p'
          then (p', e'')
          else (p', subst val var e'')
    in PosExp p t $ EMatch (subst val var e') $ map mapFn bs
  _ -> PosExp p t e

extractExp :: Exp Pos -> Exp_ Pos
extractExp (PosExp _ _ e) = e

extractVar :: Exp_ Pos -> Id
extractVar (EVar s) = s
extractVar _ = error "This shouldn't happen"

extractPatVars :: Pattern -> [Id]
extractPatVars PWildCard = []
extractPatVars (PVar s) = [s]
extractPatVars (PPair (p1,p2)) = extractPatVars p1 ++ extractPatVars p2
extractPatVars (PList (p1,p2)) = extractPatVars p1 ++ extractPatVars p2
extractPatVars (PCtor _ ps) = let f :: [Pattern] -> [Id]
                                  f = foldr ((++) . extractPatVars) []
                              in f ps
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
expToValue (PosExp _ _ (EPtr n)) = VPtr n
expToValue (PosExp _ _ (ECtor x es)) = VData x $ map expToValue es
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
isValue (PosExp _ _ (EPtr _)) = True
isValue (PosExp _ _ (ECtor _ es)) = length es == length (filter isValue es)
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

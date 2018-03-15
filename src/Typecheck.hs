module Typecheck where

import qualified Data.Map.Strict as Map

import Error
import Lang

typecheck :: Prog -> IO Typ
typecheck (d,e) = return $ typecheck' (processDecls d) e
  where processDecls :: [Decl] -> Context
        processDecls [] = Map.empty
        processDecls (DData _ cs:xs) = Map.union (Map.fromList cs) $ processDecls xs

typecheck' :: Context -> Exp Pos -> Typ
typecheck' _ (PosExp _ _ (EInt _)) = TInt
typecheck' _ (PosExp _ _ (EFloat _)) = TFloat
typecheck' _ (PosExp _ _ (EBool _)) = TBool
typecheck' _ (PosExp _ _ EUnit) = TUnit
typecheck' g (PosExp p _ (EVar s)) = case Map.lookup s g of
  (Just t') -> t'
  Nothing -> posError p "Type Error" (": " ++ s ++ " has undefined type")
typecheck' _ (PosExp _ _ ENaN) = TFloat
typecheck' g (PosExp p t (EFun (PosExp _ _ (EVar s)) e2)) = case t of
  (TArr t1 t2) -> let te2 = typecheck' (Map.insert s t1 g) e2
                  in if t2 == te2
                     then t
                     else posError p "Type Error" (": " ++ show e2 ++ " should have type " ++ show t2 ++ " but has type " ++ show te2)
  _ -> posError p "Type Error" ": function does not have an input and output type"
typecheck' g (PosExp p t (ERec (PosExp _ t' (EVar f)) (PosExp _ _ (EVar s)) e2)) = case t of
  (TArr t1 t2) -> let te2 = typecheck' (Map.insert s t1 (Map.insert f t' g)) e2
                  in if t2 == te2
                     then t
                     else posError p "Type Error" (": " ++ show e2 ++ " should have type " ++ show t2 ++ " but has type " ++ show te2)
  _ -> posError p "Type Error" ": function does not have an input and output type"
typecheck' g (PosExp p _ (EOp op e1 e2))
  | t1 == TInt && t2 == TInt = checkOp TInt
  | t1 == TFloat && (t2 == TFloat || t2 == TInt) = checkOp TFloat
  | (t1 == TInt || t1 == TFloat) && t2 == TFloat = checkOp TFloat
  | otherwise = posError p "Type Error" ": cannot perform arithmetic on non-number objects"
  where t1 = typecheck' g e1
        t2 = typecheck' g e2
        checkOp :: Typ -> Typ
        checkOp t = case op of
          Plus -> t
          Minus -> t
          Mult -> t
          Div -> t
          Mod -> t
          _ -> TBool
typecheck' g (PosExp p _ (EIf e1 e2 e3)) = if typecheck' g e1 == TBool
  then let t2 = typecheck' g e2
           t3 = typecheck' g e3
       in if t2 == t3
          then t2
          else posError p "Type Error" ": conditional branches are of different types"
  else posError p "Type Error:" ": expected a boolean value in guard of conditional"
typecheck' g (PosExp p _ (ELet (PosExp _ t (EVar s)) e2 e3)) =
  if t2 == t
  then typecheck' (Map.insert s t g) e3
  else posError p "Type Error" (": expected type of " ++ show e2 ++ " to be " ++ show t ++ " but was actually " ++ show t2)
  where t2 = typecheck' g e2
typecheck' g (PosExp p _ (EFunApp e1 e2)) = let te1 = typecheck' g e1
                                                te2 = typecheck' g e2
  in case te1 of
       (TArr t1 t2) -> if te2 == t1
         then t2
         else posError p "Type Error fun app" (": expected type of " ++ show e2 ++ " to be " ++ show t1 ++ " but was actually " ++ show te2)
       _ -> posError p "Type Error" (": expected " ++ show e1 ++ " to be a function with an arrow type but actually had type " ++ show te1)
typecheck' g (PosExp _ _ (EPair e1 e2)) = let te1 = typecheck' g e1
                                              te2 = typecheck' g e2
  in TPair te1 te2
typecheck' g (PosExp p _ (EFst e)) = let t = typecheck' g e
  in case t of
  (TPair t1 _) -> t1
  _ -> posError p "Type Error" (": expected type of " ++ show e ++ " to be a pair type but was actually " ++ show t)
typecheck' g (PosExp p _ (ESnd e)) = let t = typecheck' g e
  in case t of
  (TPair _ t2) -> t2
  _ -> posError p "Type Error" (": expected type of " ++ show e ++ " to be a pair type but was actually " ++ show t)
typecheck' _ (PosExp _ t ENil) = TList t
typecheck' g (PosExp p _ (ECons e1 e2)) = let te1 = typecheck' g e1
                                              te2 = typecheck' g e2
  in case te2 of
       (TList t) -> if t == te1
         then te2
         else posError p "Type Error" ": cons'd element is not of the same type as the rest of the list"
       _ -> posError p "Type Error" (": " ++ show e2 ++ " does not have a list type")
typecheck' g (PosExp p _ (EHead e)) = case typecheck' g e of
  (TList t) -> t
  _ -> posError p "Type Error" ": cannot take the head of a non-list"
typecheck' g (PosExp p _ (ETail e)) = case typecheck' g e of
  (TList t) -> TList t
  _ -> posError p "Type Error" ": cannot take the tail of a non-list"
typecheck' g (PosExp p _ (EEmpty e)) = case typecheck' g e of
  (TList _) -> TBool
  _ -> posError p "Type Error" ": cannot check to see if a list is empty on a non-list"
typecheck' g (PosExp _ _ (ERef e)) = TRef $ typecheck' g e
typecheck' g (PosExp p _ (ESet e1 e2)) = let te1 = typecheck' g e1
                                             te2 = typecheck' g e2
  in case te1 of
  (TRef t) -> if te2 == t
              then TUnit
              else posError p "Type Error" (": expected type of " ++ show e2 ++ " to be " ++ show t ++ " but was actually " ++ show te2)
  _ -> posError p "Type Error" ": cannot assign value to non-ref variable"
typecheck' g (PosExp p _ (EBang e)) = case typecheck' g e of
  (TRef t) -> t
  _ -> posError p "Type Error" ": cannot extract value from a non-ref variable"
typecheck' g (PosExp _ _ (ESeq _ e2)) = typecheck' g e2
typecheck' g (PosExp p _ (EWhile e1 _)) = case typecheck' g e1 of
  TBool -> TUnit
  _ -> posError p "Type Error" ": expected a boolean value in guard of while loop"
typecheck' g (PosExp p _ (ECtor i es)) = let t = constructCtorTyp es
  in case Map.lookup i g of
       (Just t') -> if t == t'
                    then case t' of
                           (TArr _ t''@(TData _)) -> t''
                           (TData _) -> t'
                           _ -> posError p "Type Error" ": invalid constructor type"
                    else posError p "Type Error" ": constructor invocation is not of the correct type"
       Nothing -> posError p "Type Error" ": constructor type unknown"
  where constructCtorTyp :: [Exp Pos] -> Typ
        constructCtorTyp = foldl (\t1 t2 -> TArr t1 $ typecheck' g t2)
          (case Map.lookup i g of
             (Just (TData s)) -> TData s
             (Just (TArr _ (TData s))) -> TData s
             Nothing -> posError p "Type Error" ": constructor unknown"
             _ -> posError p "Type Error" ": type of constructor is not given")
typecheck' g (PosExp p _ (EMatch e bs)) = let t = typecheckPattern g (typecheck' g e) bs TUnknown p
  in case t of
    TUnknown -> posError p "Type Error" ": no patterns present within pattern matching"
    _ -> t
typecheck' _ (PosExp p _ _) = posError p "Type Error" ": malformed expression reached"

typecheckPattern :: Context -> Typ -> [Branch Pos] -> Typ -> Pos -> Typ
typecheckPattern g t ((p,e):bs) prev pos = case t of
  TPair t1 t2 -> case p of
    PPair (PVar id1, PVar id2) -> let g' = Map.insert id1 t1 $ Map.insert id2 t2 g
                                      t' = typecheck' g' e
      in case prev of
      TUnknown -> typecheckPattern g t bs t' pos
      _ | prev == t' -> typecheckPattern g t bs t' pos
        | otherwise  -> posError pos "Type Error" ": expressions within pattern match do not all evaluate to same type"
    PWildCard -> let t' = typecheck' g e
      in case prev of
      TUnknown -> typecheckPattern g t bs t' pos
      _ | prev == t' -> typecheckPattern g t bs t' pos
        | otherwise  -> posError pos "Type Error" ": expressions within pattern match do not all evaluate to same type"
    _ -> posError pos "Type Error" ": pattern is not a pair type"
  TList t'    -> case p of
    PList (PVar id1, PVar id2) -> let g' = Map.insert id1 t' $ Map.insert id2 t g
                                      t'' = typecheck' g' e
      in case prev of
      TUnknown -> typecheckPattern g t bs t'' pos
      _ | prev == t'' -> typecheckPattern g t bs t'' pos
        | otherwise -> posError pos "Type Error" ": expressions within pattern match do not evaluate to same type"
    PWildCard -> let t'' = typecheck' g e
      in case prev of
      TUnknown -> typecheckPattern g t bs t'' pos
      _ | prev == t'' -> typecheckPattern g t bs t'' pos
        | otherwise  -> posError pos "Type Error" ": expressions within pattern match do not all evaluate to same type"
    _ -> posError pos "Type Error" ": pattern is not a list type"
  TData _    -> case p of
    PCtor id' ps -> case Map.lookup id' g of
      (Just t'@(TData _))
        | t /= t' ->
          posError pos "Type Error"
          (": constructor is not of type " ++ show t)
        | not $ null ps ->
          posError pos "Type Error"
          ": constructor invocation includes variables where there are none"
        | otherwise ->
          let t'' = typecheck' g e in
            case prev of
              TUnknown -> typecheckPattern g t bs t'' pos
              _ | prev == t'' -> typecheckPattern g t bs t'' pos
                | otherwise ->
                  posError pos "Type Error"
                  ": expressions within pattern match do not all evaluate to the same type"

      (Just (TArr t1 t'@(TData _)))
        | t /= t' -> 
          posError pos "Type Error" (": constructor is not of type " ++ show t)
        | t == t' && null ps ->
          posError pos "Type Error" ": constructor is only partially satisfied"
        | otherwise ->
          let f :: Pattern -> String
              f x = case x of
                (PVar s) -> s
                PWildCard -> "_"
                _ -> posError pos "Type Error" ": constructor invocation has non-id variable"
              ts = reverse $ arrToList t1
              lt = zip (map f ps) ts
          in if length lt /= length ts
             then posError pos "Type Error" ": constructor invocation is partially applied"
             else let g' = Map.union g $ Map.fromList $ filter (\(s,_) -> s /= "_") lt
                      t'' = typecheck' g' e
          in case prev of
            TUnknown -> typecheckPattern g' t bs t'' pos
            _ | prev == t'' -> typecheckPattern g' t bs t'' pos
              | otherwise -> posError pos "Type Error" ": expressions within pattern match do not all evaluate to same type"
      _ -> posError pos "Type Error" ": unknown constructor"
    PWildCard -> let t' = typecheck' g e
      in case prev of
      TUnknown -> typecheckPattern g t bs t' pos
      _ | prev == t' -> typecheckPattern g t bs t' pos
        | otherwise  -> posError pos "Type Error" ": expressions within pattern match do not all evaluate to same type"
    _ -> posError pos "Type Error" ": pattern is not a ADT type"
  _ -> posError pos "Type Error" ": cannot pattern match on a non-pair. list, or ADT expression"
typecheckPattern _ _ [] t _ = t  

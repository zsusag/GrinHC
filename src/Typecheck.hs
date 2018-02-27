module Typecheck where

import qualified Data.Map.Strict as Map

import Error
import Lang

typecheck :: Context -> Exp Pos -> IO Typ
typecheck g e = return $ typecheck' g e

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
  _ -> posError p "Type Error" ": fuction does not have an input and output type"
typecheck' g (PosExp p t (ERec (PosExp _ t' (EVar f)) (PosExp _ _ (EVar s)) e2)) = case t of
  (TArr t1 t2) -> let te2 = typecheck' (Map.insert s t1 (Map.insert f t' g)) e2
                  in if t2 == te2
                     then t
                     else posError p "Type Error" (": " ++ show e2 ++ " should have type " ++ show t2 ++ " but has type " ++ show te2)
  _ -> posError p "Type Error" ": fuction does not have an input and output type"
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
         else posError p "Type Error" (": expected type of " ++ show e2 ++ " to be " ++ show t1 ++ " but was actually " ++ show te2)
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
typecheck' _ (PosExp p _ _) = posError p "Type Error" ": malformed expression reached"

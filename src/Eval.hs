{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Eval where

import Lang
import Error

evaluate :: Exp Pos -> String
evaluate e = show simp
  where simp = simplify e

simplify :: Exp Pos -> Exp Pos
simplify (PosExp _ (EIf e1 e2 e3)) = if b then simplify e2 else simplify e3
  where b = case simplify e1 of
              (PosExp _ (EBool b')) -> b'
              (PosExp p _)  ->
                posError p "Evaluation Error" ": expected a boolean value in guard of conditional"
simplify (PosExp _ (ELeq e1 e2)) =
  case (simplify e1, simplify e2) of
    (PosExp p (EInt n1), PosExp _ (EInt n2)) -> PosExp p $ EBool $ n1 <= n2
    (PosExp p (EFloat f), PosExp _ (EInt n)) -> PosExp p $ EBool $ f <= fromIntegral n
    (PosExp p (EInt n), PosExp _ (EFloat f)) -> PosExp p $ EBool $ fromIntegral n <= f
    (PosExp p (EFloat f1), PosExp _ (EFloat f2)) -> PosExp p $ EBool $ f1 <= f2
    (PosExp p ENaN, _) -> PosExp p $ EBool False
    (_, PosExp p ENaN) -> PosExp p $ EBool False
    (PosExp _ (EInt _), PosExp p _) ->
      posError p "Evaluation Error" ": cannot compare non-number values"
    (PosExp _ (EFloat _), PosExp p _) ->
      posError p "Evaluation Error" ": cannot compare non-number values"
    (PosExp p _, PosExp _ (EInt _)) ->
      posError p "Evaluation Error" ": cannot compare non-number values"
    (PosExp p _, PosExp _ (EFloat _)) ->
      posError p "Evaluation Error" ": cannot compare non-number values"
    (PosExp p _, PosExp _ _) ->
      posError p "Evaluation Error" ": cannot compare non-number values"
simplify (PosExp _ (EOp op e1 e2)) =
  case (simplify e1, simplify e2) of
    (PosExp _ (EInt n1), PosExp p (EInt n2)) -> case op of
      Div | n2 /= 0 -> PosExp p $ EInt $ intOp op n1 n2
          | n1 == 0 -> PosExp p ENaN
          | otherwise -> posError p "Evaluation Error" ": divide by zero"
      _ -> PosExp p $ EInt $ intOp op n1 n2
    (PosExp _ (EFloat f), PosExp p (EInt n)) -> case op of
      Div | n /= 0 -> PosExp p $ EFloat $ floatOp op f (fromIntegral n)
          | f == 0 -> PosExp p ENaN
          | otherwise -> posError p "Evaluation Error" ": divide by zero"
      _ -> PosExp p $ EFloat $ floatOp op f (fromIntegral n)
    (PosExp _ (EInt n), PosExp p (EFloat f)) -> case op of
      Div | f /= 0 -> PosExp p $ EFloat $ floatOp op (fromIntegral n) f
          | n == 0 -> PosExp p ENaN
          | otherwise -> posError p "Evaluation Error" ": divide by zero"
      _ -> PosExp p $ EFloat $ floatOp op (fromIntegral n) f
    (PosExp _ (EFloat f1), PosExp p (EFloat f2)) -> case op of
      Div | f2 /= 0 -> PosExp p $ EFloat $ floatOp op f1 f2
          | f1 == 0 -> PosExp p ENaN
          | otherwise -> posError p "Evaluation Error" ": divide by zero"
      _ -> PosExp p $ EFloat $ floatOp op f1 f2
    (PosExp p ENaN, _) -> PosExp p ENaN
    (_, PosExp p ENaN) -> PosExp p ENaN
    (PosExp _ (EInt _), PosExp p _) ->
      posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
    (PosExp _ (EFloat _), PosExp p _) ->
      posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
    (PosExp p _, PosExp _ (EInt _)) ->
      posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
    (PosExp p _, PosExp _ (EFloat _)) ->
      posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
    (PosExp p _, PosExp _ _) ->
      posError p "Evaluation Error" ": cannot perform arithmetic operation on non-number values"
simplify e = e

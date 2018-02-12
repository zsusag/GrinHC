module Eval where

import Lang

evaluate :: Exp -> String
evaluate e = show simp
  where simp = simplify e

simplify :: Exp -> Exp
simplify (EAdd e1 e2 (line,col)) = let e1' = simplify e1
                                       e2' = simplify e2
  in case (e1',e2') of
       (EInt n1 _, EInt n2 p2)     -> EInt (n1 + n2) p2
       (EFloat n1 _, EInt n2 p2)   -> EFloat (n1 + fromIntegral n2) p2
       (EInt n1 _, EFloat n2 p2)   -> EFloat (fromIntegral n1 + n2) p2
       (EFloat n1 _, EFloat n2 p2) -> EFloat (n1 + n2) p2
       (ENaN p, _)              -> ENaN p
       (_, ENaN p)              -> ENaN p
       _                      -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected integer or float value")
simplify (ESub e1 e2 (line,col)) = let e1' = simplify e1
                                       e2' = simplify e2
  in case (e1',e2') of
       (EInt n1 _, EInt n2 p2)     -> EInt (n1 - n2) p2
       (EFloat n1 _, EInt n2 p2)   -> EFloat (n1 - fromIntegral n2) p2
       (EInt n1 _, EFloat n2 p2)   -> EFloat (fromIntegral n1 - n2) p2
       (EFloat n1 _, EFloat n2 p2) -> EFloat (n1 - n2) p2
       (ENaN p, _)              -> ENaN p
       (_, ENaN p)              -> ENaN p
       _                      -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected integer or float value")
simplify (EMul e1 e2 (line,col)) = let e1' = simplify e1
                                       e2' = simplify e2
  in case (e1',e2') of
       (EInt n1 _, EInt n2 p2)     -> EInt (n1 * n2) p2
       (EFloat n1 _, EInt n2 p2)   -> EFloat (n1 * fromIntegral n2) p2
       (EInt n1 _, EFloat n2 p2)   -> EFloat (fromIntegral n1 * n2) p2
       (EFloat n1 _, EFloat n2 p2) -> EFloat (n1 * n2) p2
       (ENaN p, _)              -> ENaN p
       (_, ENaN p)              -> ENaN p
       _                      -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected integer or float value")

simplify (EDiv e1 e2 (line,col)) = let e1' = simplify e1
                                       e2' = simplify e2
  in case (e1',e2') of
       (EInt n1 _, EInt n2 (line',col')) | n2 /= 0 -> EInt (n1 `div` n2) (line',col')
                                         | n1 == 0 -> ENaN (line',col')
                                         | otherwise -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line' ++ ", column " ++ show col' ++ ": Divide By Zero")
       (EFloat n1 _, EInt n2 p2)   -> EFloat (n1 / fromIntegral n2) p2
       (EInt n1 _, EFloat n2 p2)   -> EFloat (fromIntegral n1 / n2) p2
       (EFloat n1 _, EFloat n2 p2) -> EFloat (n1 / n2) p2
       (ENaN p, _)              -> ENaN p
       (_, ENaN p)              -> ENaN p
       _                  -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected integer or float value")
simplify (ELeq e1 e2 (line,col)) = let e1' = simplify e1
                                       e2' = simplify e2
  in case (e1',e2') of
       (EInt n1 _, EInt n2 p) -> EBool (n1 <= n2) p
       (EFloat n1 _, EInt n2 p)   -> EBool (n1 <= fromIntegral n2) p
       (EInt n1 _, EFloat n2 p)   -> EBool (fromIntegral n1 <= n2) p
       (EFloat n1 _, EFloat n2 p) -> EBool (n1 <= n2) p
       (ENaN p, _)              -> EBool False p
       (_, ENaN p)              -> EBool False p
       _                  -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected integer or float value")
simplify (EIf e1 e2 e3 (line,col)) = if b1 then simplify e2 else simplify e3
  where b1 = let e1' = simplify e1
             in case e1' of
                  (EBool b _) -> b
                  _         -> errorWithoutStackTrace ("Evaluation Error at line " ++ show line ++ ", column " ++ show col ++ ": expected a boolean value in guard of conditional")
simplify e = e

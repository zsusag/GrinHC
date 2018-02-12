module Lang where

type Pos = (Int,Int)

data Exp = EInt Int Pos
  | EAdd Exp Exp Pos
  | ESub Exp Exp Pos
  | EMul Exp Exp Pos
  | EDiv Exp Exp Pos
  | EBool Bool Pos
  | ELeq Exp Exp Pos
  | EIf Exp Exp Exp Pos
  | EFloat Float Pos
  | ENaN Pos
  
instance Show Exp where
  show (EInt n _)       = show n
  show (EAdd e1 e2 _)   = show e1 ++ " + " ++ show e2
  show (ESub e1 e2 _)   = show e1 ++ " - " ++ show e2
  show (EMul e1 e2 _)   = show e1 ++ " * " ++ show e2
  show (EDiv e1 e2 _)   = show e1 ++ " / " ++ show e2
  show (EBool b _)      = show b
  show (ELeq e1 e2 _)   = show e1 ++ " <= " ++ show e2
  show (EIf e1 e2 e3 _) = "if " ++ show e1 ++ " then " ++ show e2
                          ++ " else " ++ show e3
  show (EFloat f _)     = show f
  show (ENaN _)         = "NaN"

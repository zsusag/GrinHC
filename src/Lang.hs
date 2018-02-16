{-# LANGUAGE DeriveGeneric #-}
module Lang where

import GHC.Generics (Generic)

type Pos = (Int,Int)

-- Credit: Andrew Mack for helping me scraping my boilerplate
data Op = Plus | Minus | Mult | Div deriving (Generic)

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"

intOp :: Op -> (Int -> Int -> Int)
intOp Plus = (+)
intOp Minus = (-)
intOp Mult = (*)
intOp Div = div
{-# INLINE intOp #-}

floatOp :: Op -> (Float -> Float -> Float)
floatOp Plus = (+)
floatOp Minus = (-)
floatOp Mult = (*)
floatOp Div = (/)
{-# INLINE floatOp #-}

data Exp t = PosExp t (Exp_ t)


instance Show (Exp t) where
  show (PosExp _ e) = show e

data Exp_ t = EInt !Int
  | EOp !Op !(Exp t) !(Exp t)
  | EBool !Bool
  | ELeq !(Exp t) !(Exp t)
  | EIf !(Exp t) (Exp t) (Exp t)
  | EFloat !Float
  | ENaN
  deriving (Generic)

instance Show (Exp_ t) where
  show (EInt n) = show n
  show (EOp op e1 e2) = show e1 ++ " " ++  show op ++ " " ++ show e2
  show (EBool True) = "true"
  show (EBool False) = "false"
  show (ELeq e1 e2) = show e1 ++ " <= " ++ show e2
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then  " ++ show e2 ++ " else " ++ show e3
  show (EFloat f) = show f
  show ENaN = "NaN"

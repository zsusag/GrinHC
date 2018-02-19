{-# LANGUAGE DeriveGeneric #-}
module Lang where

import GHC.Generics (Generic)

type Pos = (Int,Int)

data Value = VInt {-# UNPACK #-} !Int
  | VBool !Bool
  | VFloat {-# UNPACK #-} !Float
  | VNaN
  deriving (Generic)

-- Credit: Andrew Mack for helping me scraping my boilerplate
data Op = Plus
  | Minus
  | Mult
  | Div
  | Lte
  | Geq
  | Eq
  deriving (Generic)

data Exp t = PosExp t (Exp_ t)

data Exp_ t = EInt !Int
  | EOp !Op !(Exp t) !(Exp t)
  | EBool !Bool
  | EIf !(Exp t) (Exp t) (Exp t)
  | EFloat !Float
  | ENaN
  deriving (Generic)

instance Show Value where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VFloat f)    = show f
  show VNaN          = "NaN"

instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"
  show Lte   = "<="
  show Geq   = ">="
  show Eq    = "=="

instance Show (Exp t) where
  show (PosExp _ e) = show e

instance Show (Exp_ t) where
  show (EInt n) = show n
  show (EOp op e1 e2) = show e1 ++ " " ++  show op ++ " " ++ show e2
  show (EBool True) = "true"
  show (EBool False) = "false"
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then  " ++ show e2 ++ " else " ++ show e3
  show (EFloat f) = show f
  show ENaN = "NaN"

{-# LANGUAGE DeriveGeneric #-}
module Lang where

import GHC.Generics (Generic)

type Pos = (Int,Int)

data Typ = TInt
  | TBool
  | TFloat
  | TArr !Typ !Typ
  | TUnknown
  deriving(Generic, Eq)

data Value = VInt {-# UNPACK #-} !Int
  | VBool !Bool
  | VFloat {-# UNPACK #-} !Float
  | VFun !String !(Exp Pos)
  | VRec !String !String !(Exp Pos)
  | VNaN
  deriving (Generic, Eq)

-- Credit: Andrew Mack for helping me scraping my boilerplate
data Op = Plus
  | Minus
  | Mult
  | Div
  | Lte
  | Geq
  | Eq
  | Lt
  | Gt
  | Mod
  deriving (Generic, Eq)

data Exp t = PosExp t Typ (Exp_ t)
  deriving (Eq)

data Exp_ t = EInt !Int
  | EFloat !Float
  | EBool !Bool
  | ELid !String
  | EFun !(Exp t) !(Exp t)
  | ERec !(Exp t) !(Exp t) !(Exp t)
  | ENaN
  | EOp !Op !(Exp t) !(Exp t)
  | EIf !(Exp t) (Exp t) (Exp t)
  | ELet !(Exp t) !(Exp t) !(Exp t)
  | EFunApp !(Exp t) !(Exp t)
  deriving (Generic, Eq)

instance Show Value where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VFun s v)    = "lambda " ++ s ++ " -> " ++ show v
  show (VRec f l v)  = "fix " ++ f ++ " " ++ l ++ " -> " ++ show v
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
  show Lt    = "<"
  show Gt    = ">"
  show Mod   = "%"

instance Show (Exp t) where
  show (PosExp _ _ e) = show e

instance Show (Exp_ t) where
  show (EInt n) = show n
  show (EOp op e1 e2) = "(" ++ show e1 ++ " " ++  show op ++ " " ++ show e2 ++ ")"
  show (EBool True) = "true"
  show (EBool False) = "false"
  show (ELet l e1 e2) = "let " ++ show l ++ " = " ++ show e1 ++ " in " ++ show e2
  show (EFun l e) = "lambda " ++ show l ++ " -> " ++ show e
  show (ERec f l v) = "fix " ++ show f ++ " " ++ show l ++ " -> " ++ show v
  show (EFunApp e1 e2) = show e1 ++ " " ++ show e2
  show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then  " ++ show e2 ++ " else " ++ show e3
  show (EFloat f) = show f
  show (ELid s)   = s
  show ENaN = "NaN"

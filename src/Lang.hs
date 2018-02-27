{-# LANGUAGE DeriveGeneric #-}
module Lang where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map

type Pos = (Int,Int)

type Context = Map.Map String Typ

data Typ = TInt
  | TBool
  | TFloat
  | TArr !Typ !Typ
  | TUnit
  | TPair !Typ !Typ
  | TList !Typ
  deriving(Generic)

instance NFData Typ

data Value = VInt {-# UNPACK #-} !Int
  | VBool !Bool
  | VFloat {-# UNPACK #-} !Float
  | VFun !String !(Exp Pos)
  | VRec !String !String !(Exp Pos)
  | VNaN
  | VUnit
  | VPair !Value !Value
  | VList !(Exp Pos)
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
  | EVar !String
  | EFun !(Exp t) !(Exp t)
  | ERec !(Exp t) !(Exp t) !(Exp t)
  | ENaN
  | EOp !Op !(Exp t) !(Exp t)
  | EIf !(Exp t) (Exp t) (Exp t)
  | ELet !(Exp t) !(Exp t) !(Exp t)
  | EFunApp !(Exp t) !(Exp t)
  | EUnit
  | EPair !(Exp t) !(Exp t)
  | EFst !(Exp t)
  | ESnd !(Exp t)
  | ENil
  | ECons !(Exp t) !(Exp t)
  | EHead !(Exp t)
  | ETail !(Exp t)
  | EEmpty !(Exp t)
  deriving (Generic, Eq)

instance Show Typ where
  show TInt = "Int"
  show TFloat = "Float"
  show TBool = "Bool"
  show (TArr t1 t2) = show t1 ++ " -> " ++ show t2
  show TUnit = "Unit"
  show (TPair t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TList t) = "[" ++ show t ++ "]"

instance Show Value where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show (VFun s v)    = "lambda " ++ s ++ " -> " ++ show v
  show (VRec f l v)  = "fix " ++ f ++ " " ++ l ++ " -> " ++ show v
  show (VFloat f)    = show f
  show VNaN          = "NaN"
  show VUnit         = "()"
  show (VPair v1 v2) = "(" ++ show v1 ++ ", " ++ show v2 ++ ")"
  show (VList v)     = "[" ++ show v ++ "]"

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
  show (EVar s)   = s
  show ENaN = "NaN"
  show EUnit = "()"
  show (EPair e1 e2) = "(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (EFst e) = "fst " ++ show e
  show (ESnd e) = "snd " ++ show e
  show ENil = "[]"
  show (ECons e1 e2) = case e2 of
    (PosExp _ _ ENil) -> show e1
    (PosExp _ _ (ECons _ _)) -> show e1 ++ ", " ++ show e2
    _ -> error "Element which wasn't a list was cons'd onto a list"
  show (EHead e) = "head " ++ show e
  show (ETail e) = "tail " ++ show e
  show (EEmpty e) = "empty " ++ show e

instance Eq Typ where
  TInt == TInt = True
  TFloat == TFloat = True
  TBool == TBool = True
  (TArr t1 t2) == (TArr t1' t2') = (t1 == t1' && t2 == t2') || (t1 == t2' && t2 == t1')
  TUnit == TUnit = True
  (TPair t1 t2) == (TPair t1' t2') = t1 == t1' && t2 == t2'
  (TList t1) == (TList t2) = t1 == t2
  _ == _ = False
    
